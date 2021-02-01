package inca.frontend.extensions.foreach

import inca.frontend.core.tree.Statement
import inca.frontend.extensions.foreach.Trees.Foreach
import inca.frontend.parser.CoreParser

trait Parser extends CoreParser {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override protected[frontend] def keywords: Set[String] = super.keywords ++ Seq("foreach", "in")

  override protected[frontend] def statement[_: P]: P[Statement] =
    P(
      "foreach " ~ identifier ~~ " " ~ "in " ~ exp ~ body
    ).mapWithLoc { case (s, e, b) => Foreach(s, e, b) } |
      super.statement
}
