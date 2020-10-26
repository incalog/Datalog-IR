package inca.frontend.extensions

import fastparse.ScalaWhitespace._
import fastparse._
import inca.frontend.core.Core.Exp
import inca.frontend.parser.CoreParser

/**
 * Extension adding enum expressions to @see Parser.
 */
trait EnumParser extends CoreParser {

  override def atomicExp[_: P]: P[Exp] =
    P("enum" ~ "(" ~ typeAnno ~ ")").map(Enum.apply) |
      super.atomicExp

  override def keywords: Set[String] = super.keywords + "enum"
}
