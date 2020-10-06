package inca.frontend.parser.extensions

import fastparse.ScalaWhitespace._
import fastparse._
import inca.frontend.core.Core
import inca.frontend.extensions._
import inca.frontend.parser._

/** Extension adding enum expressions to @see CoreParser.
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object EnumParser extends ParserExtension {

  override def anchorExpression: Seq[AnchorExpressionParser] = Seq(EnumParser_)

  override def keywords: Seq[String] = Seq("enum")

  object EnumParser_ extends AnchorExpressionParser {
    override def parse[_: P]: P[Core.Exp] =
      P("enum" ~ "(" ~ coreparser.typeAnno ~ ")").map(Enum(_))
  }
}
