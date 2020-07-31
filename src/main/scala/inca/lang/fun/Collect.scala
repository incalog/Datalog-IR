package inca.lang.fun

import inca.lang.fun.Fun._

object CollectVars extends Collect[String] {
  override def transAssignVars(names: Seq[Name]): Seq[String] = names
  override def transCallName(name: Name): Seq[String] = Seq(name)
  override def transParam(param: Param): Seq[String] = Seq(param.name)
  override def transAnnoParam(param: AnnoParam): Seq[String] = param.name.toSeq
  override def transVar(v: Var): Seq[String] = Seq(v.name)
}

trait Collect[R] {

  def apply(fun: PatternFunction): Seq[R] = {
    val paramStrings = fun.params.flatMap(transParam)
    val outParamStrings = fun.outParams.flatMap(transAnnoParam)
    paramStrings ++ outParamStrings ++ fun.bodies.flatMap(transAlternative)
  }

  def transParam(param: Param): Seq[R] = Seq()
  def transAnnoParam(param: AnnoParam): Seq[R] = Seq()

  def transAlternative(alt: Body): Seq[R] = alt.stmts.flatMap(transStatement)

  def transAssignVars(names: Seq[Name]): Seq[R] = Seq()

  def transStatement(stmt: Statement): Seq[R] = stmt match {
    case Assign(names, exp) => transAssignVars(names) ++ transExp(exp)
    case Assert(cond) => transCond(cond)
    case Yield(exp) => transExp(exp)
  }

  def transVar(v: Var): Seq[R] = Seq()

  def transCallName(name: Name): Seq[R] = Seq()

  def transExp(exp: Exp): Seq[R] = exp match {
    case vari@Var(name) => transVar(vari)
    case Constant(lit) => Seq()
    case PathAccess(exp, path) => transExp(exp)
    case Call(name, args, transitive, count) => transCallName(name) ++ args.flatMap(transExp)
    case Tuple(exps) => exps.flatMap(transExp)
  }

  def transCond(cond: Cond): Seq[R] = cond match {
    case Eq(lhs, rhs) => transExp(lhs) ++ transExp(rhs)
    case Neq(lhs, rhs) => transExp(lhs) ++ transExp(rhs)
    case InstanceOf(exp, typ) => transExp(exp)
    case NotInstanceOf(exp, typ) => transExp(exp)
    case Def(exp) => transExp(exp)
    case Undef(exp) => transExp(exp)
  }
}
