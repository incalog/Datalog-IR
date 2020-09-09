package inca.frontend.parser.extensions

import fastparse._
import NoWhitespace._
import inca.frontend.core.Core._
import inca.frontend.extensions._
import inca.frontend.parser.ParserUtils._
import inca.frontend.parser._
import inca.frontend.core.Core

/** Extention adding "forallexists" statements to @see CoreParser.
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object ForeachParser extends ParserExtension {

  override def statement: Seq[StatementParser] = Seq(ForeachParser_)

  override def keywords: Seq[String] = Seq("foreach", "in")

  object ForeachParser_ extends StatementParser {
    override def parse[_: P]: P[Core.Statement] =
      P(
        "foreach " ~ s_i ~ coreparser.identifier ~ " " ~ s_i ~ "in" ~ s_i ~ coreparser.exp ~ s_i ~ coreparser.body
      ).map { case (s, e, b) => Foreach(s, e, b) }
  }

}
