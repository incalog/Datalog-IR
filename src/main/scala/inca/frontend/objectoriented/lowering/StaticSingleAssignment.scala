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
class StaticSingleAssignment(val module: Module) extends ModuleLowering {

  private val gensym: Gensym = new Gensym(Iterable.empty)

  override def transModuleInternal(module: Module): Module = {
    gensym.register(module.usedModuleNames.map(_.raw))
    super.transModuleInternal(module)
  }

  override def transClassInternal(classDef: ClassDef): ClassDef = {
    gensym.register(classDef.name.raw)
    super.transClassInternal(classDef)
  }

  override def transMethodInternal(methodDef: MethodDef, classDef: ClassDef): MethodDef = gensym.scoped {
    gensym.register(methodDef.params.map(_.name.raw))
    gensym.register("this")
    env = Map()
    super.transMethodInternal(methodDef, classDef)
  }

  override def transConstructorInternal(constructorDef: ConstructorDef, classDef: ClassDef): ConstructorDef = gensym.scoped {
    gensym.register(constructorDef.params.map(_.name.raw))
    gensym.register("this")
    env = Map()
    super.transConstructorInternal(constructorDef, classDef)
  }

  type Env = Map[String, (String, Type)]
  var env: Env = Map()

  /**
   * Transform a single statement to a sequence of new statements.
   */
  override def transStatementInternal(stmt: Statement): Seq[Statement] = stmt match {
    case VarDeclareStmt(name, typ, maybeExpression, _) =>
      gensym.register(name.raw)
      val expr = if (maybeExpression.isDefined) Some(transExpression(maybeExpression.get).head) else None
      env = env + (name.raw -> (name.raw, typ))
      Seq(
        VarDeclareStmt(name, transType(typ), expr, immutable = true)
      )
    case varAssign@VarAssignStmt(targetName, expression) =>
      val targetVar = varAssign.target
        .getOrElse(throw new RuntimeException(s"Unresolved variable $targetName in assignment $varAssign"))
      targetVar match {
        case VarDeclareStmt(_, typ, _, _) =>
          val transExp = transExpression(expression).head
          val newName = gensym.fresh(targetName.raw)
          env = env + (targetName.raw -> (newName, typ))
          Seq(
            VarDeclareStmt(Name(newName), transType(typ), Some(transExp), immutable = true)
          )
        case _ =>
          throw new RuntimeException(s"Illegal assignment to variable target $targetVar")
      }
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
      val ifStmt = IfStmt(transExpression(cnd).head, thnStmts, elsStmts)

      ifStmt +: usedSymbols.zipWithIndex.map { case (name, idx) =>
        val newName = Name(gensym.fresh(name))
        val (thnName, thnType) = thnEnv.getOrElse(name, oldEnv(name))
        val (elsName, elsType) = elsEnv.getOrElse(name, oldEnv(name))
        if (thnType != elsType)
          throw new RuntimeException(s"Type mismatch for variable $newName: ${thnType} != ${elsType}")
        env = env + (name -> ((newName.raw, thnType)))
        VarPhiAssignStmt(newName, thnType, ifStmt, Name(thnName), Name(elsName))
      }.toSeq
    case stmt =>
      gensym.register(stmt.vars.map(_._1.raw))
      super.transStatementInternal(stmt)
  }

  override def transExpressionInternal(expression: Expression): Seq[Expression] = expression match {
    case VarReadExpr(targetName) =>
      // Rewrite all VarReadExpr to use the latest generated name for the variable
      val (newName, _) = env.getOrElse(targetName.raw, (targetName.raw, TAny))
      Seq(VarReadExpr(Name(newName)))
    case _ => super.transExpressionInternal(expression)
  }
}
