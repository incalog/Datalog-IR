package inca.frontend.extensions

import inca.frontend.core.Core.{Assign, Eval, Exp, Name, Statement, TString, TypeAnno, Var}
import inca.frontend.desugar.{DesugarTrans, Desugarable}
import inca.util.Gensym

import scala.collection.mutable.ListBuffer

// TODO except a list of args. This way we only emit a single eval node instead of multiple ones
case class Concat(lhs: Exp, rhs: Exp) extends Exp {
  def freeVars: Map[Name, Option[TypeAnno]] = lhs.freeVars ++ rhs.freeVars
  def prettyprint(implicit indent: String): String = s"${lhs.prettyprint} ++ ${rhs.prettyprint}"
}

case class Contains(lhs: Exp, rhs: Exp) extends Exp {
  def freeVars: Map[Name, Option[TypeAnno]] = lhs.freeVars ++ rhs.freeVars
  def prettyprint(implicit indent: String): String = s"contains(${lhs.prettyprint}, ${rhs.prettyprint})"
}

object StringOps extends Desugarable {
  override val desugarsTo: Seq[Desugarable] = Seq()
  override def trans(): DesugarTrans = new DesugarTrans {
    val concatStatements: ListBuffer[Statement] = ListBuffer()

    override def desugarExp(exp: Exp)(implicit gensym: Gensym): Exp = exp match {
      case Concat(lhs, rhs) =>
        val lhssym = gensym.fresh("lhs")
        val rhssym = gensym.fresh("rhs")
        val ressym = gensym.fresh("res")
        concatStatements += Assign(Seq(lhssym), desugarExp(lhs))
        concatStatements += Assign(Seq(rhssym), desugarExp(rhs))
        concatStatements += Assign(Seq(ressym), Eval(Seq(lhssym, rhssym), TString, s"$lhssym ++ $rhssym"))
        changed(Var(ressym).typed(TString))
      case _ => super.desugarExp(exp)
    }

    override def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = {
      val desugared = super.desugarStm(stm)
      if (concatStatements.isEmpty)
        desugared
      else {
        val prepend = concatStatements.toSeq
        concatStatements.clear()
        prepend ++ desugared
      }
    }
  }
}
