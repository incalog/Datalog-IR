package inca.frontend.extensions

import inca.frontend.Frontend
import inca.frontend.core.Core._
import inca.frontend.desugar.{DesugarTrans, Desugarable}
import inca.frontend.parser.SourceLocation
import inca.frontend.typechecker.{NoTerminator, StmType}
import inca.util.Gensym

import scala.collection.mutable.ListBuffer

case class IfThenElse(cond: Exp, thn: Body, elseIfs: Seq[ElseIf], els: Option[Body]) extends Statement {
  override def boundVars: Set[Name] = thn.boundVars ++ elseIfs.flatMap(_.boundVars) ++ els.toSeq.flatMap(_.boundVars)
  override def allVars: Map[Name, Option[TypeAnno]] = cond.freeVars ++ thn.allVars ++ elseIfs.flatMap(_.allVars) ++ els.toSeq.flatMap(_.allVars)

  override def prettyprint(implicit indent: String): String = {
    val elseIfsS = elseIfs.map(_.prettyprint).mkString("\n")
    val elseS = if (els.isEmpty) "" else " " + els.get.prettyprint
    s"${indent}if (${cond.prettyprint}) ${thn.prettyprint}$elseIfsS$elseS".stripMargin
  }
}
case class ElseIf(cond: Exp, body: Body) extends SourceLocation {
  def boundVars: Set[Name] = body.boundVars
  def allVars: Map[Name, Option[TypeAnno]] = cond.freeVars ++ body.allVars
  def prettyprint(implicit indent: String): String =
    s" else if (${cond.prettyprint}) ${body.prettyprint}".stripMargin
}

/**
 * Extension adding "ifthenelse" statements to @see Parser.
 */
trait IfThenElseFrontend extends Frontend {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override protected def desugarables: Seq[Desugarable] = IfThenElse +: super.desugarables

  /**
   * Statement parser
   */
  override protected[frontend] def statement[_: P]: P[Statement] = {
    ifThenElse |
      super.statement
  }

  protected[frontend] def ifThenElse[_: P]: P[Statement] =
    P(
      "if" ~ "(" ~ exp ~ ")" ~ body ~
        elseif.rep.? ~ P("else" ~ body).?
    ).mapWithLoc { case (e, b, eifs, el) => IfThenElse(e, b, eifs.getOrElse(Seq.empty), el) }

  protected[frontend] def elseif[_: P]: P[ElseIf] =
    P(
      "else" ~ "if" ~ "(" ~ exp ~ ")" ~ body
    ).mapWithLoc { case (e, b) => ElseIf(e, b) }


  override protected[frontend] def keywords: Set[String] = super.keywords ++ Seq("if", "else")

  override protected def typecheckInternal(stm: Statement, mustTerminate: Boolean): StmType = stm match {
    case IfThenElse(cond, thn, elseIfs, els) =>
      val conds = cond +: elseIfs.map(_.cond)
      conds.foreach { c =>
        val ty = typecheck(c)
        if (ty != TBool)
          error(s"Expected condition of type $TBool, but found $ty", c)
      }

      val bodies = thn +: elseIfs.map(_.body)
      val bodyTypes = bodies.map { b =>
        typecheck(b, mustTerminate)
      }

      val elsTy = els.map(typecheck(_, mustTerminate)).getOrElse(NoTerminator)
      bodyTypes.foldLeft(elsTy)(_.meet(_, lang))

    case _ => super.typecheckInternal(stm, mustTerminate)
  }
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
