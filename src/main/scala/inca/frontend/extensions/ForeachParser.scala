package inca.frontend.extensions

import fastparse.ScalaWhitespace._
import fastparse._
import inca.frontend.core.Core.Statement
import inca.frontend.parser.CoreParser

/**
 * Extension adding "forallexists" statements to @see Parser.
 */
trait ForeachParser extends CoreParser {
  override def statement[_: P]: P[Statement] =
    P(
      "foreach " ~ identifier ~~ " " ~ "in " ~ exp ~ body
    ).map { case (s, e, b) => Foreach(s, e, b) } |
      super.statement

  override def keywords: Set[String] = super.keywords ++ Seq("foreach", "in")
}
