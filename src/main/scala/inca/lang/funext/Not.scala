package inca.lang.funext

import inca.lang.fun.Fun._
import inca.lang.funext.desugar.{DesugarTrans, Desugarable}
import inca.util.Gensym

case class Not(cond: Exp) extends Exp {
  override def usedvars: Map[Name, Option[TypeAnno]] = cond.usedvars

  override def prettyprint(implicit indent: String): String =
    s"!(${cond.prettyprint})"
}

object Not extends Desugarable {
  override def trans(): DesugarTrans = new DesugarTrans {
    override def desugarExp(cond: Exp)(implicit gensym: Gensym): Exp = cond match {
      case Not(cond) => desugarNot(desugarExp(cond))
      case _ => super.desugarExp(cond)
    }

    def desugarNot(cond: Exp)(implicit gensym: Gensym): Exp = cond match {
      case Eq(lhs, rhs) => changed(Neq(lhs, rhs))
      case Neq(lhs, rhs) => changed(Eq(lhs, rhs))
      case InstanceOf(exp, typ) => changed(NotInstanceOf(exp, typ))
      case NotInstanceOf(exp, typ) => changed(InstanceOf(exp, typ))
      case Def(exp) => changed(Undef(exp))
      case Undef(exp) => changed(Def(exp))
      case Constant(BooleanLiteral(v)) => changed(Constant(BooleanLiteral(!v)))
      case Eval(vars, ty, code) => changed(Eval(vars, ty, s"!{$code}"))
      case Not(cond) => changed(cond)
      case _ => Not(cond)
    }
  }
}
