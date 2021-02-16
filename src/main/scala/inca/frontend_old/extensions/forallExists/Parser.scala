package inca.frontend_old.extensions.forallExists

import inca.frontend_old.core.tree.Statement
import inca.frontend_old.extensions.forallExists.Trees._
import inca.frontend_old.parser.CoreParser

trait Parser extends CoreParser {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override protected[frontend_old] def statement[_: P]: P[Statement] =
    P("forall " ~ identifier ~~ " " ~ "in " ~ exp ~ body).mapWithLoc { case (name, expression, body) => Forall(name, expression, body) } |
      P("exists " ~ identifier ~~ " " ~ "in " ~ exp ~ body).mapWithLoc { case (name, expression, body) => Exists(name, expression, body) } |
      super.statement

  override protected[frontend_old] def keywords: Set[String] = super.keywords ++ Seq("forall", "exists", "in")
}
