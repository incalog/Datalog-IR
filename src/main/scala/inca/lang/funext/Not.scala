package inca.lang.funext

import inca.lang.fun.Fun._
import inca.lang.funext.desugar.{DesugarTrans, Desugarable}
import inca.util.Gensym

case class Not(cond: Cond) extends Cond {
  override def usedvars: Map[Name, Option[TypeAnno]] = cond.usedvars

  override def prettyprint(implicit indent: String): String =
    s"!(${cond.prettyprint})"
}

object Not extends Desugarable {
  override def trans(): DesugarTrans = new DesugarTrans {
    override def desugarCond(cond: Cond)(implicit gensym: Gensym): Cond = cond match {
      case Not(cond) => desugarNot(desugarCond(cond))
      case _ => super.desugarCond(cond)
    }

    def desugarNot(cond: Cond)(implicit gensym: Gensym): Cond = cond match {
      case Eq(lhs, rhs) => changed(Neq(lhs, rhs))
      case Neq(lhs, rhs) => changed(Eq(lhs, rhs))
      case InstanceOf(exp, typ) => changed(NotInstanceOf(exp, typ))
      case NotInstanceOf(exp, typ) => changed(InstanceOf(exp, typ))
      case Def(exp) => changed(Undef(exp))
      case Undef(exp) => changed(Def(exp))
      case BooleanCond(v) => changed(BooleanCond(!v))
      case Not(cond) => changed(cond)
      case _ => Not(cond)
    }
  }
}
