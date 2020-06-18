package inca.lang

import inca.MetaElements.Link
import inca.lang.FunLang._

object ExtLang {
  case class Cast(src: Exp, typ: Type) extends Exp

  case class Switch(alts: Seq[Alternative]) extends Statement
  case class StatementList(stmts: Seq[Statement]) extends Statement

  // if
  case class If(cond: Cond, elif: Seq[ElseIf], els: Seq[Statement]) extends Statement
  case class ElseIf(cond: Cond, body: Seq[Statement])

  // TODO foreach

  // pattern match
  case class Match(matchee: Exp, cases: Seq[Case]) extends Statement
  case class Case(pattern: Seq[Pattern], body: Seq[Statement])

  trait Pattern

  case class NodePattern(c: TType, bindings: Seq[PatternBinding]) extends Pattern
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
}
