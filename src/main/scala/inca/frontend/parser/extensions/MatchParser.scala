package inca.frontend.parser.extensions

import fastparse._
import NoWhitespace._
import inca.frontend.core.Core._
import inca.frontend.extensions._
import inca.frontend.parser.ParserUtils._
import inca.frontend.parser._
import inca.frontend.core.Core

/** Extention adding pattern matching statements to @see CoreParser.
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
        coreparser.identifier ~ s_i ~ "=" ~ pattern
      ).map { case (s, p) => PatternBinding(s, p) }

    def nodePattern[_: P]: P[Pattern] =
      P(
        coreparser.tNode ~ s_i ~ "(" ~ P(s_i ~ patternBinding ~ s_i).rep(sep = ",") ~ ")"
      ).map { case (tn, pbs) => NodePattern(tn, pbs) }

    def tuplePattern[_: P]: P[Pattern] =
      P(s_i ~ "(" ~ s_i ~ P(s_i ~ pattern ~ s_i).rep(sep = ",") ~ ")" ~ s_i)
        .map(TuplePattern(_))

    def varPattern[_: P]: P[Pattern] =
      P(s_i ~ coreparser.identifier ~ s_i).map(VarPattern(_))

    def namedPattern[_: P]: P[Pattern] =
      P(s_i ~ coreparser.identifier ~ "@" ~ pattern).map {
        case (n, p) => NamedPattern(n, p)
      }

    def wildcardPattern[_: P]: P[Pattern] =
      P(s_i ~ "_").!.map(_ => WildcardPattern)

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
      P(s_i ~ "case " ~ s_i ~ pattern ~ s_i ~ "=>" ~ s_i ~ coreparser.body).map {
        case (p, b) => Case(p, b)
      }

    override def parse[_: P]: P[Statement] =
      P(
        coreparser.exp ~ " " ~ s_i ~ "match" ~ s_i ~ "{" ~ sn_i ~ P(s_i ~ case_ ~ s_i)
          .rep(sep = n_) ~ sn_i ~ "}"
      ).map { case (e, cs) => Match(e, cs) }
  }
}
