package inca.frontend_old.extensions.match_

import inca.frontend_old.parser.CoreParser
import inca.frontend_old.parser.ParserUtils.{nl_!, sp}

trait Parser extends CoreParser {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override val syntax: Syntax
  import syntax._

  override protected[frontend_old] def keywords: Set[String] = super.keywords ++ Seq("match", "case")

  /**
   * Statement parser
   */
  override protected[frontend_old] def statement[_: P]: P[Statement] =
    matchStatement | super.statement

  protected[frontend_old] def matchStatement[_: P]: P[Statement] =
    P(exp ~ "match" ~ "{" ~ P(sp ~~ case_ ~~ sp).repX(sep = nl_!) ~ "}").mapWithLoc { case (e, cs) => Match(e, cs) }

  protected[frontend_old] def case_[_: P]: P[Case] =
    P("case " ~ pattern ~ "=>" ~ body).mapWithLoc { case (pattern, body) => Case(pattern, body) }

  protected[frontend_old] def pattern[_: P]: P[Pattern] =
    P(tuplePattern | namedPattern | nodePattern | wildcardPattern | varPattern
      | literalPattern | scalaPattern)

  protected[frontend_old] def patternBinding[_: P]: P[PatternBinding] =
    P(identifier ~ "=" ~ pattern).mapWithLoc { case (name, pattern) => PatternBinding(name, pattern)} |
      P(identifier).mapWithLoc(name => PatternBinding(name, VarPattern(name)))

  protected[frontend_old] def nodePattern[_: P]: P[Pattern] =
    P(tNode ~ "(" ~ P(patternBinding).rep(sep = ",") ~ ")").mapWithLoc { case (node, bindings) => NodePattern(node, bindings)}

  protected[frontend_old] def scalaPattern[_: P]: P[Pattern] =
    P("`" ~~ evalCore ~~ "`" ~
      ("(" ~ pattern.rep(sep = ",") ~ ")").?).flatMapWithLoc { case (ev, ps) =>
      Pass(ScalaPattern(ev, ps.isEmpty, ps.getOrElse(Seq())))
    }

  protected[frontend_old] def tuplePattern[_: P]: P[Pattern] =
    P("(" ~ P(pattern).rep(sep = ",") ~ ")").mapWithLoc(TuplePattern)

  protected[frontend_old] def varPattern[_: P]: P[Pattern] =
    P(identifier).mapWithLoc(VarPattern)

  protected[frontend_old] def namedPattern[_: P]: P[Pattern] =
    P(identifier ~ "@" ~ pattern).mapWithLoc { case (name, pattern) => NamedPattern(name, pattern)}

  protected[frontend_old] def wildcardPattern[_: P]: P[Pattern] =
    P("_").!.mapWithLoc(_ => WildcardPattern)

  protected[frontend_old] def literalPattern[_: P]: P[Pattern] =
    P(literal).mapWithLoc(LiteralPattern)

}
