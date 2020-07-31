package inca.lang.funext

import inca.lang.fun.Fun._
import inca.lang.funext.desugar.{DesugarTrans, Desugarable}

case class Match(matchee: Exp, cases: Seq[Case]) extends Statement {
  override def usedvars: Set[Name] = ???

  override def prettyprint(implicit indent: String): String = ???
}
case class Case(pattern: Seq[Pattern], body: Seq[Statement])

sealed trait Pattern

case class NodePattern(c: TNode, bindings: Seq[PatternBinding]) extends Pattern
case class PatternBinding(pattern: Pattern, link: Link)

case class VarPattern(name: Name) extends Pattern
case class NamedPattern(varpat: VarPattern, pat: Pattern) extends Pattern

case object DefaultPattern extends Pattern
case object WildcardPattern extends Pattern

case class BoolPattern(v: BooleanLiteral) extends Pattern
case class IntPattern(v: IntLiteral) extends Pattern
case class LongPattern(v: LongLiteral) extends Pattern
case class DoublePattern(v: DoubleLiteral) extends Pattern
case class StringPattern(v: StringLiteral) extends Pattern

object Match extends Desugarable {
  override def trans(): DesugarTrans = ???
}