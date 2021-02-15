package inca.frontend_old.extensions.ifThenElse

import inca.frontend_old.core.tree._
import inca.frontend_old.desugar.{DesugarTrans, Desugarable}
import inca.frontend_old.extensions.ifThenElse.Trees._
import inca.frontend_old.extensions.{boolOps, switch_}
import inca.util.Gensym

import scala.collection.mutable.ListBuffer

object Desugaring extends Desugarable {

  override val desugarsTo: Seq[Desugarable] = Seq(switch_.Desugaring, boolOps.Desugaring)
  import boolOps.Trees._
  import switch_.Trees._

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

    def desugarConditional(cond: Expression, notconds: Iterable[Not], body: Seq[Statement])(implicit gensym: Gensym): Seq[Statement] =
      notconds.toSeq.map(Assert) ++ Seq(Assert(cond)) ++ body
  }
}