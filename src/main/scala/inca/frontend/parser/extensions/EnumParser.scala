package inca.frontend.parser.extensions

import fastparse._
import NoWhitespace._
import inca.frontend.core.Core._
import inca.frontend.extensions._
import inca.frontend.parser.ParserUtils._
import inca.frontend.parser._
import inca.frontend.core.Core

/** Extention adding enum expressions to @see CoreParser.
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object EnumParser extends ParserExtension {

  override def anchorExpression: Seq[AnchorExpressionParser] = Seq(EnumParser_)

  override def keywords: Seq[String] = Seq("enum")

  object EnumParser_ extends AnchorExpressionParser {
    override def parse[_: P]: P[Core.Exp] =
      P(
        "enum" ~ s_i ~ "(" ~ coreparser.typeAnno ~ ")"
      ).map(Enum(_))
  }
}
