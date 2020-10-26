package inca.frontend.extensions

import fastparse.ScalaWhitespace._
import fastparse._
import inca.frontend.core.Core.Statement
import inca.frontend.parser.CoreParser

/**
 * Extension adding "forallexists" statements to @see Parser.
 */
trait ForallExistsParser extends CoreParser {

  override def statement[_: P]: P[Statement] =
    P("forall " ~ identifier ~~ " " ~ "in " ~ exp ~ body).map(Forall.tupled) |
      P("exists " ~ identifier ~~ " " ~ "in " ~ exp ~ body).map(Exists.tupled) |
      super.statement

  override def keywords: Set[String] = super.keywords ++ Seq("forall", "exists", "in")
}
