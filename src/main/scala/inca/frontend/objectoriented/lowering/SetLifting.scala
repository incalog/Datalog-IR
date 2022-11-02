package inca.frontend.objectoriented.lowering

import inca.frontend.objectoriented.core._
import inca.util.Gensym

/**
 * If a constructor is used inside a SetExpr, then the same object with different values is created, since each
 * body of the SetExpr uses the same allocation counter. This behaviour is wrong, since multiple objects should be
 * created for each element in the set. To prevent this error, all expressions inside a SetExpr are lifted outside the
 * SetExpr as VarDeclare statements.
 *
 * Example:
 * Before:
 *  return [new Num(1), new Num(2), new Num(3)]
 *
 * After:
 *  val tmp$0 = new Num(1)
 *  val tmp$1 = new Num(2)
 *  val tmp$2 = new Num(3)
 *  return [tmp$0, tmp$1, tmp$2]
 *
 */
class SetLifting(val module: Module) extends ModuleLowering {
  private val gensym: Gensym = new Gensym(Iterable.empty)

  class GenStmt(var stmts: Seq[Statement] = Seq()) {
    def scoped[A](f: => A): A = {
      val oldstmts = this.stmts
      this.stmts = Seq()
      try {
        val a = f
        a
      } finally {
        this.stmts = oldstmts
      }
    }

    def add(stmts: Seq[Statement]): Unit = {
      this.stmts ++= stmts
    }
  }

  private val genStmt: GenStmt = new GenStmt()

  override def transClassInternal(classDef: ClassDef): ClassDef = {
    gensym.register(classDef.name.raw)
    super.transClassInternal(classDef)
  }

  override def transMethodInternal(methodDef: MethodDef, classDef: ClassDef): MethodDef = gensym.scoped {
    gensym.register(methodDef.params.map(_.name.raw))
    gensym.register("this")
    super.transMethodInternal(methodDef, classDef)
  }

  override def transConstructorInternal(constructorDef: ConstructorDef, classDef: ClassDef): ConstructorDef = gensym.scoped {
    gensym.register(constructorDef.params.map(_.name.raw))
    gensym.register("this")
    super.transConstructorInternal(constructorDef, classDef)
  }

  override def transStatementInternal(stmt: Statement): Seq[Statement] = genStmt.scoped {
    val transStmt = stmt match {
      case VarDeclareStmt(name, _, _, _) =>
        gensym.register(name.raw)
        super.transStatementInternal(stmt)
      case VarAssignStmt(_, _) =>
        throw new IllegalArgumentException("Set lifting failed! Encountered unexpected var assignment.")
      case VarPhiAssignStmt(name, _, _, _, _) =>
        gensym.register(name.raw)
        super.transStatementInternal(stmt)
      case _ => super.transStatementInternal(stmt)
    }
    genStmt.stmts ++ transStmt
  }

  private def liftSetExpression(set: SetExpr): (Expression, Seq[Statement]) = {
    val innerExps = set.exps.map(e => gensym.fresh("tmp") -> e)
    val setExpr = SetExpr(innerExps.map(tup => VarReadExpr(Name(tup._1))), set.tty)
    val liftedExps = innerExps.map {
      case (n, e) => VarDeclareStmt(Name(n), e.typ.get, Some(transExpression(e).head), immutable = true)
    }
    (setExpr, liftedExps)
  }

  override def transExpressionInternal(expression: Expression): Seq[Expression] = expression match {
    case setExpr@SetExpr(_, _) =>
      val (newExpr, stmts) = liftSetExpression(setExpr)
      genStmt.add(stmts)
      Seq(newExpr)
    case _ => super.transExpressionInternal(expression)
  }
}


object SetLifting {
  def transformModule(module: Module): Module = {
    new SetLifting(module).transModule()
  }

  def transformModules(modules: Seq[Module]): Seq[Module] =
    modules.map(transformModule)
}