package inca.frontend.constraint.core

import inca.compiler.SourceLocation
import inca.util.Meta

case class Body(stmts: Seq[Statement]) extends SourceLocation {
  def boundVars: Set[Name] = stmts.flatMap(_.boundVars).toSet

  def allVars: Map[Name, Option[Type]] = stmts.flatMap(_.allVars).toMap

  def freeVars: Map[Name, Option[Type]] = allVars -- boundVars


  override def toString: String = prettyprint("")

  def prettyprint(implicit indent: String): String = {
    val stmtsS = if (stmts.isEmpty) " " else
      "\n" + stmts.map(_.prettyprint(indent + Meta.TAB)).mkString("\n")
    s"""{$stmtsS
       |$indent}""".stripMargin
  }
}

object Body {
  def empty: Body = new Body(Seq())

  def apply(stmt: Statement, stmts: Statement*): Body = new Body(stmt +: stmts)
}