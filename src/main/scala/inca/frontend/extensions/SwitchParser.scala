package inca.frontend.extensions

import fastparse.ScalaWhitespace._
import fastparse._
import inca.frontend.core.Core.Statement
import inca.frontend.parser.CoreParser

/**
 * Extension adding "switch" statements to @see Parser.
 */
trait SwitchParser extends CoreParser {

  override def keywords: Set[String] = super.keywords + "switch"

  override def statement[_: P]: P[Statement] =
    P("switch" ~ body.rep(sep = "union")).map { bodies =>
      if (bodies.size == 1 && bodies.head.stmts.isEmpty) Switch(Seq.empty)
      else Switch(bodies)
    } |
      super.statement
}
