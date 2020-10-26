package inca.frontend.extensions

import fastparse.ScalaWhitespace._
import fastparse._
import inca.frontend.core.Core.Exp
import inca.frontend.parser.CoreParser

/**
 * Extension adding cast expressions to @see Parser.
 */
trait CastParser extends CoreParser {
  override def trailExp[_: P]: P[Exp => Exp] =
    P(":" ~ typeAnno).map(ty => Cast(_, ty)) |
      super.trailExp
}
