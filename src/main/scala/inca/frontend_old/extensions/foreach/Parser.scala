package inca.frontend_old.extensions.foreach

import inca.frontend_old.parser.CoreParser

trait Parser extends CoreParser {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override val syntax: Syntax
  import syntax._

  override protected[frontend_old] def keywords: Set[String] = super.keywords ++ Seq("foreach", "in")

  override protected[frontend_old] def statement[_: P]: P[Statement] =
    P(
      "foreach " ~ identifier ~~ " " ~ "in " ~ exp ~ body
    ).mapWithLoc { case (s, e, b) => Foreach(s, e, b) } |
      super.statement
}
