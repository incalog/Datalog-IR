package inca.frontend.parser.extensions

import fastparse._
import NoWhitespace._
import inca.frontend.core.Core._
import inca.frontend.extensions._
import inca.frontend.parser.ParserUtils._
import inca.frontend.parser._
import inca.frontend.core.Core

/** Extension adding cast expressions to @see CoreParser.
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object CastParser extends ParserExtension {

  override def recursiveExpression: Seq[RecursiveExpressionParser] = Seq(CastParser_)

  object CastParser_ extends RecursiveExpressionParser {
    override def parse[_: P](e: Core.Exp): P[Core.Exp] =
      P(
        sp ~ ":" ~ sp ~ coreparser.typeAnno
      ).map(Cast(e, _))
  }

}
