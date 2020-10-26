package inca.frontend.extensions

import fastparse.ScalaWhitespace._
import fastparse._
import inca.frontend.core.Core.Statement
import inca.frontend.parser.CoreParser
import inca.frontend.parser.ParserUtils._

/**
 * Extension adding pattern matching statements to @see CoreParser.
 */
trait MatchParser extends CoreParser {

  override def keywords: Set[String] = super.keywords ++ Seq("match", "case")

  /**
   * Statement parser
   */
  override def statement[_: P]: P[Statement] =
    matchStatement | super.statement

  def matchStatement[_: P]: P[Statement] =
    P(exp ~ "match" ~ "{" ~ P(sp ~~ case_ ~~ sp).repX(sep = nl_!) ~ "}").map { case (e, cs) => Match(e, cs) }

  def case_[_: P]: P[Case] =
    P("case " ~ pattern ~ "=>" ~ body).map(Case.tupled)

  def pattern[_: P]: P[Pattern] =
    P(tuplePattern | namedPattern | nodePattern | wildcardPattern | varPattern
      | literalPattern)

  def patternBinding[_: P]: P[PatternBinding] =
    P(identifier ~ "=" ~ pattern).map(PatternBinding.tupled)

  def nodePattern[_: P]: P[Pattern] =
    P(tNode ~ "(" ~ P(patternBinding).rep(sep = ",") ~ ")").map(NodePattern.tupled)

  def tuplePattern[_: P]: P[Pattern] =
    P("(" ~ P(pattern).rep(sep = ",") ~ ")").map(TuplePattern)

  def varPattern[_: P]: P[Pattern] =
    P(identifier).map(VarPattern)

  def namedPattern[_: P]: P[Pattern] =
    P(identifier ~ "@" ~ pattern).map(NamedPattern.tupled)

  def wildcardPattern[_: P]: P[Pattern] =
    P("_").!.map(_ => WildcardPattern)

  def literalPattern[_: P]: P[Pattern] =
    P(literal).map(LiteralPattern)

}
