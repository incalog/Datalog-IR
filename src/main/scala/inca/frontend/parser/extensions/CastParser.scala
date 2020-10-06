package inca.frontend.parser.extensions

import fastparse.ScalaWhitespace._
import fastparse._
import inca.frontend.core.Core
import inca.frontend.extensions._
import inca.frontend.parser._

/** Extension adding cast expressions to @see CoreParser.
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object CastParser extends ParserExtension {

  override def recursiveExpression: Seq[RecursiveExpressionParser] = Seq(CastParser_)

  object CastParser_ extends RecursiveExpressionParser {
    override def parse[_: P](e: Core.Exp): P[Core.Exp] =
      P(":" ~ coreparser.typeAnno).map(Cast(e, _))
  }

}
