package inca.frontend_old.extensions.match_

import inca.frontend_old.core
import inca.frontend_old.parser.SourceLocation

trait Syntax extends core.Syntax {
  type Case <: SourceLocation
  type Pattern <: SourceLocation
  type PatternBinding <: SourceLocation

  def Match(matchee: Expression, cases: Seq[Case]): Statement
  def Case(pattern: Pattern, body: Body): Case
  def PatternBinding(field: Name, pattern: Pattern): PatternBinding

  def NodePattern(c: TNode, bindings: Seq[PatternBinding]): Pattern
  def ScalaPattern(fun: Eval, noArgs: Boolean, args: Seq[Pattern]): Pattern
  def TuplePattern(pats: Seq[Pattern]): Pattern
  def VarPattern(name: Name): Pattern
  def NamedPattern(name: Name, pat: Pattern): Pattern
  def WildcardPattern: Pattern
  def LiteralPattern(v: Literal): Pattern
}
