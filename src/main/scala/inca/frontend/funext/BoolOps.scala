package inca.frontend.funext

import inca.frontend.desugar.{DesugarTrans, Desugarable}
import inca.frontend.fun.Fun._
import inca.util.Gensym

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Not(cond: Exp) extends Exp {
  override def freeVars: Map[Name, Option[TypeAnno]] = cond.freeVars
  override def prettyprint(implicit indent: String): String = s"!(${cond.prettyprint})"
}
case class And(e1: Exp, e2: Exp) extends Exp {
  override def freeVars: Map[Name, Option[TypeAnno]] = e1.freeVars ++ e2.freeVars
  override def prettyprint(implicit indent: String): String = s"(${e1.prettyprint} && ${e2.prettyprint})"
}
case class Or(e1: Exp, e2: Exp) extends Exp {
  override def freeVars: Map[Name, Option[TypeAnno]] = e1.freeVars ++ e2.freeVars
  override def prettyprint(implicit indent: String): String = s"(${e1.prettyprint} || ${e2.prettyprint})"
}


object BoolOps extends Desugarable {

  override val desugarsTo: Seq[Desugarable] = Seq(Switch)

  override def trans(): DesugarTrans = new DesugarTrans {
    val booltatements: ListBuffer[Statement] = ListBuffer()
    val orAlternatives: mutable.MultiDict[String, Exp] = mutable.MultiDict()

    override def desugarExp(cond: Exp)(implicit gensym: Gensym): Exp = cond match {
      case Not(cond) => desugarNot(cond).orTyped(TBool)
      case And(e1, e2) =>
        booltatements += Assert(desugarExp(e1).orTyped(TBool))
        changed(desugarExp(e2).orTyped(TBool))
      case Or(e1, e2) =>
        val sym = gensym.fresh("or")
        orAlternatives += sym -> e1.orTyped(TBool)
        orAlternatives += sym -> e2.orTyped(TBool)
        changed(Var(sym))
      case _ => super.desugarExp(cond)
    }

    def desugarNot(cond: Exp)(implicit gensym: Gensym): Exp = cond match {
      case Not(cond) => changed(desugarExp(cond))
      case And(e1, e2) => changed(desugarExp(Or(Not(e1), Not(e2))))
      case Or(e1, e2) => changed(desugarExp(And(Not(e1), Not(e2))))

      case Eq(lhs, rhs) => changed(Neq(desugarExp(lhs), desugarExp(rhs)))
      case Neq(lhs, rhs) => changed(Eq(desugarExp(lhs), desugarExp(rhs)))
      case InstanceOf(exp, typ) => changed(NotInstanceOf(desugarExp(exp), typ))
      case NotInstanceOf(exp, typ) => changed(InstanceOf(desugarExp(exp), typ))
      case Def(exp) => changed(Undef(desugarExp(exp)))
      case Undef(exp) => changed(Def(desugarExp(exp)))
      case Constant(BooleanLiteral(v)) => changed(Constant(BooleanLiteral(!v)))
      case Eval(vars, ty, code) => changed(Eval(vars, ty, s"!{$code}"))

      case _ => Not(cond)
    }

    override def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = {
      val desugared = super.desugarStm(stm)
      val stms =
        if (booltatements.isEmpty)
          desugared
        else {
          val prepend = booltatements.toSeq
          booltatements.clear()
          prepend ++ desugared
        }
      if (orAlternatives.isEmpty)
        stms
      else {
        val alts = orAlternatives.toSeq.map { case (sym, exp) =>
          Body(Assign(Seq(sym), exp) +: stms)
        }
        orAlternatives.clear()
        Seq(Switch(alts))
      }
    }
  }
}
