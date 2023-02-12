package inca.frontend.constraint.extensions.match_

import inca.frontend.constraint.core._
import inca.frontend.constraint.extensions.match_.Trees._
import inca.frontend.constraint.parser.CoreParser
import inca.frontend.util.ParserUtils.{nl_!, sp}

trait Parser extends CoreParser {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override protected[frontend] def keywords: Set[String] = super.keywords ++ Seq("match", "case")

  /**
   * Statement parser
   */
  override protected[frontend] def statement[_: P]: P[Statement] =
    matchStatement | super.statement

  protected[frontend] def matchStatement[_: P]: P[Statement] =
    P(exp ~ "match" ~ "{" ~ P(sp ~~ case_ ~~ sp).repX(sep = nl_!) ~ "}").mapWithLoc { case (e, cs) => Match(e, cs) }

  protected[frontend] def case_[_: P]: P[Case] =
    P("case " ~ pattern ~ "=>" ~ body).mapWithLoc { case (pattern, body) => Case(pattern, body) }

  protected[frontend] def pattern[_: P]: P[Pattern] =
    P(tuplePattern | namedPattern | nodePattern | wildcardPattern | varPattern
      | literalPattern | scalaPattern)

  protected[frontend] def patternBinding[_: P]: P[PatternBinding] =
    P(identifier ~ "=" ~ pattern).mapWithLoc { case (name, pattern) => PatternBinding(name, pattern)} |
      P(identifier).mapWithLoc(name => PatternBinding(name, VarPattern(name)))

  protected[frontend] def nodePattern[_: P]: P[Pattern] =
    P(tNode ~ "(" ~ P(patternBinding).rep(sep = ",") ~ ")").mapWithLoc { case (node, bindings) => NodePattern(node, bindings)}

  protected[frontend] def scalaPattern[_: P]: P[Pattern] =
    P("`" ~~ evalCore ~~ "`" ~
      ("(" ~ pattern.rep(sep = ",") ~ ")").?).flatMapWithLoc { case (ev, ps) =>
      Pass(ScalaPattern(ev, ps.isEmpty, ps.getOrElse(Seq())))
    }

  protected[frontend] def tuplePattern[_: P]: P[Pattern] =
    P("(" ~ P(pattern).rep(sep = ",") ~ ")").mapWithLoc(TuplePattern)

  protected[frontend] def varPattern[_: P]: P[Pattern] =
    P(identifier).mapWithLoc(VarPattern)

  protected[frontend] def namedPattern[_: P]: P[Pattern] =
    P(identifier ~ "@" ~ pattern).mapWithLoc { case (name, pattern) => NamedPattern(name, pattern)}

  protected[frontend] def wildcardPattern[_: P]: P[Pattern] =
    P("_").!.mapWithLoc(_ => WildcardPattern)

  protected[frontend] def literalPattern[_: P]: P[Pattern] =
    P(literal).mapWithLoc(LiteralPattern)

}
