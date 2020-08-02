package inca.lang.funext

import inca.lang.fun.Fun._
import inca.lang.funext.desugar.{DesugarTrans, Desugarable}
import inca.util.Gensym
import inca.util.Meta.TAB

import scala.collection.mutable.ListBuffer

case class IfThenElse(cond: Cond, thn: Seq[Statement], elseIfs: Seq[ElseIf], els: Option[Seq[Statement]]) extends Statement {
  override def usedvars: Set[Name] = cond.usedvars ++ thn.flatMap(_.usedvars) ++ els.toSeq.flatMap(_.flatMap(_.usedvars))

  override def prettyprint(implicit indent: String): String = {
    val thnS = if (thn.isEmpty) "" else
      "\n" + thn.map(_.prettyprint(indent+TAB)).mkString("\n")
    val elseIfsS = elseIfs.map(_.prettyprint).mkString("\n")
    val elseS = if (els.isEmpty) "" else {
      val elsStmtsS = if (els.get.isEmpty) "" else
        "\n" + els.get.map(_.prettyprint(indent+TAB)).mkString("\n")
      s""" else {$elsStmtsS
         |${indent}}""".stripMargin
    }
    s"""${indent}if (${cond.prettyprint}) {$thnS
       |${indent}}$elseIfsS$elseS""".stripMargin
  }
}
case class ElseIf(cond: Cond, body: Seq[Statement]) {
  def usedvars: Set[Name] = cond.usedvars ++ body.flatMap(_.usedvars)
  def prettyprint(implicit indent: String): String = {
    val bodyS = if (body.isEmpty) "" else
      "\n" + body.map(_.prettyprint(indent+TAB)).mkString("\n")
    s""" else if (${cond.prettyprint}) {$bodyS
       |${indent}}""".stripMargin
  }
}

object IfThenElse extends Desugarable {
  override val desugarsTo: Set[Desugarable] = Set(Switch, Not)

  override def trans(): DesugarTrans = new DesugarTrans {

    override def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = stm match {
      case IfThenElse(cond, thn, elseIfs, els) =>
        val thnBody = Body(desugarConditional(cond, ListBuffer(), thn.flatMap(desugarStm)))
        val notconds = ListBuffer(Not(cond))
        val elseIfBodies = elseIfs.map { elseIf =>
          val elseIfBody = Body(desugarConditional(elseIf.cond, notconds, elseIf.body.flatMap(desugarStm)))
          notconds += Not(elseIf.cond)
          elseIfBody
        }
        val elseBody = els match {
          case Some(stms) => Body(notconds.toSeq.map(Assert) ++ stms.flatMap(desugarStm))
          case None => Body(Seq())
        }
        changed(Seq(Switch(thnBody +: (elseIfBodies :+ elseBody))))

      case _ => super.desugarStm(stm)
    }

    def desugarConditional(cond: Cond, notconds: ListBuffer[Not], body: Seq[Statement])(implicit gensym: Gensym): Seq[Statement] =
      notconds.toSeq.map(Assert) ++ Seq(Assert(cond)) ++ body
  }
}
