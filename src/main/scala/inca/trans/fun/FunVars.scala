package inca.trans.fun

import inca.lang.FunLang.{Alternative, Assert, Assignment, Call, Cond, Constant, Def, Eq, Exp, InstanceOf, Neq, NotInstanceOf, PathAccess, PatternFunction, Return, Statement, Tuple, Undef, Var}

object FunVars {

  def apply(fun: PatternFunction): Seq[String] = {
    val paramStrings = fun.params.map(_.name)
    val outParamStrings = fun.outParams.flatMap(_.name)
    paramStrings ++ outParamStrings ++ fun.bodies.flatMap(transAlternative)
  }

  def transAlternative(alt: Alternative): Seq[String] = alt.stmts.flatMap(transStatement)

  def transStatement(stmt: Statement): Seq[String] = stmt match {
    case Assignment(names, exp) => names ++ transExp(exp)
    case Assert(cond) => transCond(cond)
    case Return(exp) => transExp(exp)
  }

  def transExp(exp: Exp): Seq[String] = exp match {
    case Var(name) => Seq(name)
    case Constant(lit) => Seq()
    case PathAccess(exp, path) => transExp(exp)
    case Call(call, count) => Seq(call.name) ++ call.args.flatMap(transExp)
    case Tuple(exps) => exps.flatMap(transExp)
  }

  def transCond(cond: Cond): Seq[String] = cond match {
    case Eq(lhs, rhs) => transExp(lhs) ++ transExp(rhs)
    case Neq(lhs, rhs) => transExp(lhs) ++ transExp(rhs)
    case InstanceOf(exp, typ) => transExp(exp)
    case NotInstanceOf(exp, typ) => transExp(exp)
    case Def(exp) => transExp(exp)
    case Undef(exp) => transExp(exp)
  }
}
