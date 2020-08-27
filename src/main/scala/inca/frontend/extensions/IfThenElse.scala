package inca.frontend.extensions

import inca.frontend.core.Core._
import inca.frontend.desugar.{DesugarTrans, Desugarable}
import inca.util.Gensym

import scala.collection.mutable.ListBuffer

case class IfThenElse(cond: Exp, thn: Body, elseIfs: Seq[ElseIf], els: Option[Body]) extends Statement {
  override def boundVars: Set[Name] = thn.boundVars ++ elseIfs.flatMap(_.boundVars) ++ els.toSeq.flatMap(_.boundVars)
  override def allVars: Map[Name, Option[TypeAnno]] = cond.freeVars ++ thn.allVars ++ elseIfs.flatMap(_.allVars) ++ els.toSeq.flatMap(_.allVars)

  override def prettyprint(implicit indent: String): String = {
    val elseIfsS = elseIfs.map(_.prettyprint).mkString("\n")
    val elseS = if (els.isEmpty) "" else " " + els.get.prettyprint
    s"${indent}if (${cond.prettyprint}) $thn$elseIfsS$elseS".stripMargin
  }
}
case class ElseIf(cond: Exp, body: Body) {
  def boundVars: Set[Name] = body.boundVars
  def allVars: Map[Name, Option[TypeAnno]] = cond.freeVars ++ body.allVars
  def prettyprint(implicit indent: String): String =
    s" else if (${cond.prettyprint}) ${body.prettyprint}".stripMargin
}

object IfThenElse extends Desugarable {
  override def trans(): DesugarTrans = new DesugarTrans {

    override def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = stm match {
      case IfThenElse(cond, thn, elseIfs, els) =>
        val thnBody = Body(desugarConditional(cond, Seq(), thn.stmts.flatMap(desugarStm)))
        val notconds = ListBuffer(Not(cond))
        val elseIfBodies = elseIfs.map { elseIf =>
          val elseIfBody = Body(desugarConditional(elseIf.cond, notconds, elseIf.body.stmts.flatMap(desugarStm)))
          notconds += Not(elseIf.cond)
          elseIfBody
        }
        val elseBody = els match {
          case Some(body) => Body(notconds.toSeq.map(Assert) ++ body.stmts.flatMap(desugarStm))
          case None => Body(Seq())
        }
        changed(Seq(Switch(thnBody +: (elseIfBodies :+ elseBody))))

      case _ => super.desugarStm(stm)
    }

    def desugarConditional(cond: Exp, notconds: Iterable[Not], body: Seq[Statement])(implicit gensym: Gensym): Seq[Statement] =
      notconds.toSeq.map(Assert) ++ Seq(Assert(cond)) ++ body
  }

  override val desugarsTo: Seq[Desugarable] = Seq(Switch, BoolOps)
}
