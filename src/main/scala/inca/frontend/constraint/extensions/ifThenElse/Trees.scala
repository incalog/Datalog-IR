package inca.frontend.constraint.extensions.ifThenElse

import inca.compiler.SourceLocation
import inca.frontend.constraint.core.tree._

object Trees {
  case class IfThenElse(cond: Expression, thn: Body, elseIfs: Seq[ElseIf], els: Option[Body]) extends Statement {
    override def boundVars: Set[Name] = thn.boundVars ++ elseIfs.flatMap(_.boundVars) ++ els.toSeq.flatMap(_.boundVars)
    override def allVars: Map[Name, Option[Type]] = cond.freeVars ++ thn.allVars ++ elseIfs.flatMap(_.allVars) ++ els.toSeq.flatMap(_.allVars)

    override def prettyprint(implicit indent: String): String = {
      val elseIfsS = elseIfs.map(_.prettyprint).mkString("\n")
      val elseS = if (els.isEmpty) "" else " " + els.get.prettyprint
      s"${indent}if (${cond.prettyprint}) ${thn.prettyprint}$elseIfsS$elseS".stripMargin
    }
  }
  case class ElseIf(cond: Expression, body: Body) extends SourceLocation {
    def boundVars: Set[Name] = body.boundVars
    def allVars: Map[Name, Option[Type]] = cond.freeVars ++ body.allVars
    def prettyprint(implicit indent: String): String =
      s" else if (${cond.prettyprint}) ${body.prettyprint}".stripMargin
  }

}
