package inca.frontend.constraint.extensions.forallExists

import inca.frontend.constraint.core.Statement
import inca.frontend.constraint.extensions.forallExists.Trees._
import inca.frontend.constraint.parser.CoreParser

trait Parser extends CoreParser {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override protected[frontend] def statement[_: P]: P[Statement] =
    P("forall " ~ identifier ~~ " " ~ "in " ~ exp ~ body).mapWithLoc { case (name, expression, body) => Forall(name, expression, body) } |
      P("exists " ~ identifier ~~ " " ~ "in " ~ exp ~ body).mapWithLoc { case (name, expression, body) => Exists(name, expression, body) } |
      super.statement

  override protected[frontend] def keywords: Set[String] = super.keywords ++ Seq("forall", "exists", "in")
}
