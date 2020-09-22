package inca.frontend.parser.extensions

import fastparse._
import ScalaWhitespace._
import inca.frontend.core.Core._
import inca.frontend.extensions._
import inca.frontend.parser.ParserUtils._
import inca.frontend.parser._
import inca.frontend.core.Core

/** Extension adding pattern matching statements to @see CoreParser.
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object MatchParser extends ParserExtension {

  override def keywords: Seq[String] = Seq("match", "case")

  override def statement: Seq[StatementParser] = Seq(MatchParser_)

  object MatchParser_ extends StatementParser {

    def patternBinding[_: P]: P[PatternBinding] =
      P(
        coreparser.identifier ~ "=" ~ pattern
      ).map { case (s, p) => PatternBinding(s, p) }

    def nodePattern[_: P]: P[Pattern] =
      P(
        coreparser.tNode ~ "(" ~ P(patternBinding).rep(sep = ",") ~ ")"
      ).map { case (tn, pbs) => NodePattern(tn, pbs) }

    def tuplePattern[_: P]: P[Pattern] =
      P("(" ~ P(pattern).rep(sep = ",") ~ ")")
        .map(TuplePattern(_))

    def varPattern[_: P]: P[Pattern] =
      P(coreparser.identifier).map(VarPattern(_))

    def namedPattern[_: P]: P[Pattern] =
      P(coreparser.identifier ~ "@" ~ pattern).map {
        case (n, p) => NamedPattern(n, p)
      }

    def wildcardPattern[_: P]: P[Pattern] =
      P("_").!.map(_ => WildcardPattern)

    def literalPattern[_: P]: P[Pattern] =
      P(coreparser.literal).map(LiteralPattern(_))

    def pattern[_: P]: P[Pattern] =
      P(
        tuplePattern
          | namedPattern
          | nodePattern
          | varPattern
          | wildcardPattern
          | literalPattern
      )

    def case_[_: P]: P[Case] =
      P("case " ~ pattern ~ "=>" ~ coreparser.body).map {
        case (p, b) => Case(p, b)
      }

    override def parse[_: P]: P[Statement] =
      P(
        coreparser.exp ~ "match" ~ "{" ~ P(sp ~~ case_ ~~ sp).repX(sep = nl_!) ~ "}"
      ).map { case (e, cs) => Match(e, cs) }
  }
}
