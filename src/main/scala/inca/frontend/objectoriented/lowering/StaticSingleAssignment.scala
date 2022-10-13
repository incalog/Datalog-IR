package inca.frontend.objectoriented.lowering

import inca.compiler.SourceLocation
import inca.frontend.objectoriented.core._
import inca.util.Gensym


object StaticSingleAssignment {
  def transformModule(module: Module): Module =
    new StaticSingleAssignment(module).transModule()

  def transformModules(modules: Seq[Module]): Seq[Module] =
    modules.map(transformModule)
}

/**
 * This class performs a static single assignment transformation on the AST. Each VarAssignStmt and mutable
 * VarDeclareStmt is replaced by an immutable VarDeclareStmt. Each declaration has it's own unique name postfixed
 * by a $ and a number. After all IfStmts, for each contained variable in the then- and else-body a new VarPhiAssignStmt
 * is inserted, that stores the last variable name of the then-block and the else-block.
 *
 * All currently resolved targets from the previous typechecking are cleared by traversing the AST and creating new
 * instances of all resolvable objects.
 */
class StaticSingleAssignment(module: Module) {

  private val gensym: Gensym = new Gensym(Iterable.empty)

  private def preserveLoc[U <: SourceLocation](loc: U)(f: U => U): U = {
    val transLoc = f(loc)
    transLoc.startIndex = loc.startIndex
    transLoc.endIndex = loc.endIndex
    transLoc
  }

  private def preserveLocs[U <: SourceLocation](loc: U)(f: U => Seq[U]): Seq[U] = {
    f(loc).map { transLoc =>
      transLoc.startIndex = loc.startIndex
      transLoc.endIndex = loc.endIndex
      transLoc
    }
  }

  protected[frontend] def transModule(): Module = preserveLoc(module)(transModuleInternal)

  private def transModuleInternal(module: Module): Module = {
    val Module(name, imports, classes) = module
    gensym.register(module.usedModuleNames.map(_.raw))

    val transClasses = classes.map(transClass)

    Module(name, imports, transClasses)
  }

  protected[frontend] def transClass(classDef: ClassDef): ClassDef = preserveLoc(classDef)(transClassInternal)

  private def transClassInternal(classDef: ClassDef): ClassDef = {
    gensym.register(classDef.name.raw)
    val ClassDef(annos, vis, name, parents, content) = classDef
    val newContent = content.map(c => transContent(c, classDef))
    ClassDef(annos, vis, name, parents.map(c => ClassRef(c.name)), newContent)
  }

  protected[frontend] def transContent(content: ClassContent, classDef: ClassDef): ClassContent = content match {
    case constructor: ConstructorDef => preserveLoc(constructor)(c => transConstructorInternal(c, classDef))
    case method: MethodDef => preserveLoc(method)(m => transMethodInternal(m, classDef))
    case field: FieldDef => preserveLoc(field)(f => transFieldInternal(f, classDef))
  }

  private def transFieldInternal(fieldDef: FieldDef, classDef: ClassDef): FieldDef =
    gensym.scoped {
      val FieldDef(annos, vis, name, typ, body, immutable) = fieldDef
      FieldDef(annos, vis, name, typ, body, immutable)
    }

  private def transMethodInternal(methodDef: MethodDef, classDef: ClassDef): MethodDef =
    gensym.scoped {
      gensym.register(methodDef.params.map(_.name.raw))
      gensym.register("this")
      env = Map()
      val MethodDef(annos, vis, name, params, outType, body) = methodDef
      val newBody = transStatements(body)
      MethodDef(annos, vis, name, params, outType, newBody)
    }

  private def transConstructorInternal(constructorDef: ConstructorDef, classDef: ClassDef): ConstructorDef =
    gensym.scoped {
      gensym.register(constructorDef.params.map(_.name.raw))
      gensym.register("this")
      env = Map()
      val ConstructorDef(annos, vis, params, body) = constructorDef
      val newBody = transStatements(body)
      ConstructorDef(annos, vis, params, newBody)
    }

  type Env = Map[String, (String, Type)]
  var env: Env = Map()

  protected[frontend] def transStatements(stmts: Seq[Statement]): Seq[Statement] =
    stmts.flatMap(transStatement)

  protected[frontend] def transStatement(stmt: Statement): Seq[Statement] =
    preserveLocs(stmt)(transStatementInternal)

  private def transStatementInternal(stmt: Statement): Seq[Statement] =
    stmt match {
      case VarDeclareStmt(name, typ, maybeExpression, _) =>
        gensym.register(name.raw)
        val expr = if (maybeExpression.isDefined) Some(transExpression(maybeExpression.get)) else None
        env = env + (name.raw -> (name.raw, typ))
        Seq(
          VarDeclareStmt(name, transType(typ), expr, immutable = true)
        )
      case varAssign@VarAssignStmt(targetName, expression) =>
        val targetVar = varAssign.target
          .getOrElse(throw new RuntimeException(s"Unresolved variable $targetName in assignment $varAssign"))
        targetVar match {
          case VarDeclareStmt(_, typ, _, _) =>
            val transExp = transExpression(expression)
            val newName = gensym.fresh(targetName.raw)
            env = env + (targetName.raw -> (newName, typ))
            Seq(
              VarDeclareStmt(Name(newName), transType(typ), Some(transExp), immutable = true)
            )
          case _ =>
            throw new RuntimeException(s"Illegal assignment to variable target $targetVar")
        }
      case ReturnStmt(expr) =>
        gensym.register(stmt.vars.map(_._1.raw))
        Seq(ReturnStmt(transExpression(expr)))
      case ExprStmt(expr) =>
        gensym.register(stmt.vars.map(_._1.raw))
        Seq(ExprStmt(transExpression(expr)))
      case FieldAssignStmt(recv, name, expression) =>
        gensym.register(stmt.vars.map(_._1.raw))
        Seq(FieldAssignStmt(transExpression(recv), name, transExpression(expression)))
      case IfStmt(cnd, thn, els) =>
        val oldEnv = env
        val thnStmts = transStatements(thn)
        val thnEnv = env.filter(kv => oldEnv.contains(kv._1) && !oldEnv.get(kv._1).contains(kv._2))
        env = oldEnv
        val elsStmts = transStatements(els)
        val elsEnv = env.filter(kv => oldEnv.contains(kv._1) && !oldEnv.get(kv._1).contains(kv._2))
        env = oldEnv

        // base name of all new symbols either used in the then or the else block
        val usedSymbols = thnEnv.keySet.union(elsEnv.keySet)
        val ifStmt = IfStmt(transExpression(cnd), thnStmts, elsStmts)

        ifStmt +: usedSymbols.zipWithIndex.map { case (name, idx) =>
          val newName = Name(gensym.fresh(name))
          val (thnName, thnType) = thnEnv.getOrElse(name, oldEnv(name))
          val (elsName, elsType) = elsEnv.getOrElse(name, oldEnv(name))
          if (thnType != elsType)
            throw new RuntimeException(s"Type mismatch for variable $newName: ${thnType} != ${elsType}")
          env = env + (name -> ((newName.raw, thnType)))
          // set a unique source location for this assignment. It doesn't matter which
          VarPhiAssignStmt(newName, thnType, ifStmt, Name(thnName), Name(elsName))
        }.toSeq
      case s =>
        throw new RuntimeException(s"Can not transform statement: $s")
    }

  protected[frontend] def transExpression(expression: Expression): Expression =
    preserveLoc(expression)(transExpressionInternal)

  private def transExpressionInternal(expression: Expression): Expression =
    expression match {
      case FieldReadExpr(recv, targetName) =>
        FieldReadExpr(transExpression(recv), targetName)
      case VarReadExpr(targetName) =>
        // Rewrite all VarReadExpr to use the latest generated name for the variable
        val (newName, _) = env.getOrElse(targetName.raw, (targetName.raw, TAny))//throw new RuntimeException(s"Unregistered variable $targetName encountered!"))
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
      case TupleReadExpr(recv, index) =>
        TupleReadExpr(transExpression(recv), index)
      case SetExpr(exps) =>
        SetExpr(exps.map(transExpression))
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


  protected[frontend] def transType(typ: Type): Type = preserveLoc(typ)(transTypeInternal)

  private def transTypeInternal(typ: Type): Type = {
    typ match {
      case TAny => TAny
      case TNull => TNull
      case TTuple(ts) => TTuple(ts.map(transType))
      case TSet(ty) => TSet(transType(ty))
      case TScala(ty) => TScala(ty)
      // create a new ClassRef to invalidate the current target
      case TClass(ClassRef(name)) => TClass(ClassRef(name))
    }
  }
}
