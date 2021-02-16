package inca.frontend_old.extensions.match_

import inca.frontend_old.core.tree._
import inca.frontend_old.parser.SourceLocation
import inca.frontend_old.typechecker.Typeable
import inca.util.Meta

object Trees {
  case class Match(matchee: Expression, cases: Seq[Case]) extends Statement {
    override def boundVars: Set[Name] = cases.flatMap(_.boundVars).toSet
    override def allVars: Map[Name, Option[Type]] = matchee.freeVars ++ cases.flatMap(_.allVars)

    override def prettyprint(implicit indent: String): String = {
      val casesS = if (cases.isEmpty) "" else
        "\n" + cases.map(_.prettyprint(indent+Meta.TAB)).mkString("\n")
      s"""${indent}${matchee.prettyprint} match {$casesS
         |${indent}}""".stripMargin
    }

  }
  case class Case(pattern: Pattern, body: Body) extends SourceLocation {
    def boundVars: Set[Name] = pattern.boundVars ++ body.boundVars
    def allVars: Map[Name, Option[Type]] = pattern.allVars ++ body.allVars

    def prettyprint(implicit indent: String): String =
      s"${indent}case ${pattern.prettyprint} => ${body.prettyprint}"
  }

  sealed trait Pattern extends SourceLocation with Typeable {
    def boundVars: Set[Name]
    def allVars: Map[Name, Option[Type]]
    def prettyprint(implicit indent: String): String
  }

  case class NodePattern(c: TNode, bindings: Seq[PatternBinding]) extends Pattern {
    def boundVars: Set[Name] = bindings.flatMap(_.pattern.boundVars).toSet
    override def allVars: Map[Name, Option[Type]] = bindings.flatMap(_.pattern.allVars).toMap

    override def prettyprint(implicit indent: String): String = {
      val bindingsS = if (bindings.isEmpty) "" else
        bindings.map(_.prettyprint).mkString(", ")
      s"${c.prettyprint}($bindingsS)"
    }
  }
  case class PatternBinding(field: Name, pattern: Pattern) extends Typeable with SourceLocation {
    def prettyprint(implicit indent: String): String =
      s"$field = ${pattern.prettyprint}"
  }

  case class ScalaPattern(fun: Eval, noArgs: Boolean, args: Seq[Pattern]) extends Pattern {

    def boundVars: Set[Name] = args.flatMap(_.boundVars).toSet
    override def allVars: Map[Name, Option[Type]] = args.flatMap(_.allVars).toMap

    override def prettyprint(implicit indent: String): String = {
      val argsS = if (args.isEmpty) "" else
        args.map(_.prettyprint).mkString(", ")
      s"${fun.code.syntax}($argsS)"
    }
  }

  case class TuplePattern(pats: Seq[Pattern]) extends Pattern {
    override def boundVars: Set[Name] = pats.flatMap(_.boundVars).toSet
    override def allVars: Map[Name, Option[Type]] = pats.flatMap(_.allVars).toMap
    override def prettyprint(implicit indent: String): String =
      if (pats.isEmpty)
        "()"
      else if (pats.size == 1)
        pats.head.prettyprint
      else
        pats.map(_.prettyprint).mkString("(", ", ", ")")
  }

  case class VarPattern(name: Name) extends Pattern with Var.Target {
    override def boundVars: Set[Name] = Set(name)
    override def allVars: Map[Name, Option[Type]] = Map(name -> None)
    override def prettyprint(implicit indent: String): String = name.name
  }
  case class NamedPattern(name: Name, pat: Pattern) extends Pattern with Var.Target {
    override def boundVars: Set[Name] = Set(name) ++ pat.boundVars
    override def allVars: Map[Name, Option[Type]] = Map(name -> None) ++ pat.allVars
    override def prettyprint(implicit indent: String): String = s"$name@${pat.prettyprint}"
  }

  case object WildcardPattern extends Pattern {
    override def boundVars: Set[Name] = Set()
    override def allVars: Map[Name, Option[Type]] = Map()
    override def prettyprint(implicit indent: String): String = "_"
  }

  case class LiteralPattern(v: Literal) extends Pattern {
    override def boundVars: Set[Name] = Set()
    override def allVars: Map[Name, Option[Type]] = Map()
    override def prettyprint(implicit indent: String): String = v.prettyprint
  }
}
