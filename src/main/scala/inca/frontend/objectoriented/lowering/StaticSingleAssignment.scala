package inca.frontend.objectoriented.lowering

import inca.frontend.objectoriented.core._
import inca.frontend.objectoriented.util.GensymTyped
import inca.util.Gensym

import scala.collection.mutable.ListBuffer


object StaticSingleAssignment {
  def transformModule(module: Module): Module =
    new StaticSingleAssignment(module).transModule()

  def transformModules(modules: Seq[Module]): Seq[Module] =
    modules.map(transformModule)
}

// Note:
// Make sure to create a new instance for each resolvable object in order to clear the targets ! This class rewrites
// part of the AST, which will invalidate the targets!
class StaticSingleAssignment(module: Module) {

  private val gensym: GensymTyped = new GensymTyped(Iterable.empty)

  def transModule(): Module = {
    val Module(name, imports, classes) = module
    gensym.register(module.usedModuleNames.map(_.raw))

    val transClasses = classes.map(transClass)

    Module(name, imports, transClasses)
  }

  private def transClass(classDef: ClassDef): ClassDef = {
    gensym.registerWithType(classDef.name.raw, Some(TClass(ClassRef(classDef.name))))

    val ClassDef(annos, vis, name, parents, content) = classDef
    val transContent = content.map {
      case constructor: ConstructorDef => transConstructor(constructor, classDef)
      case method: MethodDef => transMethod(method, classDef)
      case c => c
    }
    ClassDef(annos, vis, name, parents.map(c => ClassRef(c.name)), transContent)
  }

  private def transMethod(methodDef: MethodDef, classDef: ClassDef): MethodDef = gensym.scoped {
    gensym.registerWithTypes(methodDef.params.map(p => p.name.raw -> Some(p.typ)))
    gensym.registerWithType("this", Some(classDef.typ))
    val MethodDef(annos, vis, name, params, outType, body) = methodDef
    val newBody = transStatements(body)
    MethodDef(annos, vis, name, params, outType, newBody)
  }

  private def transConstructor(constructorDef: ConstructorDef, classDef: ClassDef): ConstructorDef = gensym.scoped {
    gensym.registerWithTypes(constructorDef.params.map(p => p.name.raw -> Some(p.typ)))
    gensym.registerWithType("this", Some(classDef.typ))
    val ConstructorDef(annos, vis, params, body) = constructorDef
    val newBody = transStatements(body)
    ConstructorDef(annos, vis, params, newBody)
  }

  private def transStatements(stmts: Seq[Statement]): Seq[Statement] = {
    stmts.flatMap(transStatement)
  }

  private def transStatement(stmt: Statement): Seq[Statement] = {
    stmt match {
      case VarDeclareStmt(name, typ, maybeExpression, immutable) =>
        gensym.registerWithType(name.raw, Some(typ))
        val expr = if (maybeExpression.isDefined) Some(transExpression(maybeExpression.get)) else None
        Seq(
          VarDeclareStmt(name, transType(typ), expr, immutable = true)
        )
      case varAssign@VarAssignStmt(targetName, expression) =>
        val targetVar = varAssign.target
          .getOrElse(throw new RuntimeException(s"Unresolved variable $targetName in assignment $varAssign"))
        targetVar match {
          case VarDeclareStmt(_, typ, _, _) =>
            Seq(
              VarDeclareStmt(Name(gensym.fresh(targetName.raw)), transType(typ), Some(transExpression(expression)), immutable = true)
            )
          case _ =>
            throw new RuntimeException(s"Illegal assignment to variable target $targetVar")
        }
      case ReturnStmt(expr) =>
        gensym.registerWithTypes(stmt.vars.map { case (k, v) => k.raw -> v })
        Seq(ReturnStmt(transExpression(expr)))
      case ExprStmt(expr) =>
        gensym.registerWithTypes(stmt.vars.map { case (k, v) => k.raw -> v })
        Seq(ExprStmt(transExpression(expr)))
      case FieldAssignStmt(recv, name, expression) =>
        gensym.registerWithTypes(stmt.vars.map { case (k, v) => k.raw -> v })
        Seq(FieldAssignStmt(transExpression(recv), name, transExpression(expression)))
      case IfStmt(cnd, thn, els) =>
        val beforeGenSym = gensym.snapshot

        val thnStmts = transStatements(thn)
        val thnGenSym = gensym.diff(beforeGenSym)

        val elsStmts = transStatements(els)
        val elsGenSym = gensym.diff(thnGenSym.union(beforeGenSym))

        // base name of all new symbols either used in the thn or the else block
        val usedSymbols = thnGenSym.union(elsGenSym).symbols
        val ifStmt = IfStmt(transExpression(cnd), thnStmts, elsStmts)

        ifStmt +: usedSymbols.map { name =>
          val newName = Name(gensym.fresh(name))
          val (thnName, thnType) = thnGenSym.get(name).getOrElse(beforeGenSym.get(name).get)
          val (elsName, elsType) = elsGenSym.get(name).getOrElse(beforeGenSym.get(name).get)
          if (thnType.isEmpty || elsType.isEmpty)
            throw new RuntimeException(s"Type for variable $newName is not defined!")
          if (thnType != elsType)
            throw new RuntimeException(s"Type mismatch for variable $newName: ${thnType} != ${elsType}")

          VarPhiAssignStmt(newName, thnType.get, ifStmt, Name(thnName), Name(elsName))
        }.toSeq
      case s =>
        throw new RuntimeException(s"Can not transform statement: $s")
    }
  }

  private def transExpression(expression: Expression): Expression = {
    expression match {
      case FieldReadExpr(recv, targetName) =>
        FieldReadExpr(transExpression(recv), targetName)
      case VarReadExpr(targetName) =>
        // Rewrite all VarReadExpr to use the latest generated name for the variable
        val (newName, _) = gensym.get(targetName.raw)
          .getOrElse(throw new RuntimeException(s"Unregistered variable $targetName encountered!"))
        VarReadExpr(Name(newName))
      case ConstructorExpr(ClassRef(name), args) =>
        ConstructorExpr(ClassRef(name), args.map(transExpression))
      case SuperExpr(args) =>
        SuperExpr(args.map(transExpression))
      case MethodCallExpr(recv, fun, args) =>
        MethodCallExpr(transExpression(recv), fun, args.map(transExpression))
      case TypeCastExpr(recv, toTyp) =>
        TypeCastExpr(transExpression(recv), transType(toTyp))
      case InstanceOfExpr(recv, ofTyp) =>
        InstanceOfExpr(transExpression(recv), transType(ofTyp))
      case EqualsExpr(obj1, obj2) =>
        EqualsExpr(transExpression(obj1), transExpression(obj2))
      case TupleExpr(exps) =>
        TupleExpr(exps.map(transExpression))
      case BaseApplyExpr(fun, args) =>
        BaseApplyExpr(fun, args.map(transExpression))
      case BaseApplyInfixExpr(left, op, right) =>
        BaseApplyInfixExpr(transExpression(left), op, transExpression(right))
      case BaseApplyMethodExpr(recv, method, args) =>
        BaseApplyMethodExpr(transExpression(recv), method, Some(args.getOrElse(Seq()).map(transExpression)))
      case BaseApplyUnaryExpr(op, exp) =>
        BaseApplyUnaryExpr(op, transExpression(exp))
      case NullExpr() =>
        NullExpr()
      case BaseLitExpr(code) =>
        BaseLitExpr(code)
      case expr =>
        throw new RuntimeException(s"Can not transform expression: $expr")
    }
  }

  def transType(typ: Type): Type = {
    typ match {
      case TAny => TAny
      case TNull => TNull
      case TTuple(ts) => TTuple(ts.map(transType))
      case TScala(ty) => TScala(ty)
      // create a new ClassRef to invalidate the current target
      case TClass(ClassRef(name)) => TClass(ClassRef(name))
    }
  }
}
