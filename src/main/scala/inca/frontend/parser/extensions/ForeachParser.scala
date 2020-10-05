package inca.frontend.parser.extensions

import fastparse.ScalaWhitespace._
import fastparse._
import inca.frontend.core.Core
import inca.frontend.extensions._
import inca.frontend.parser._

/** Extension adding "forallexists" statements to @see CoreParser.
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
        "foreach " ~ coreparser.identifier ~~ " " ~ "in" ~ coreparser.exp ~ coreparser.body
      ).map { case (s, e, b) => Foreach(s, e, b) }
  }

}
