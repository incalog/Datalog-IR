package inca.frontend.parser.extensions

import fastparse.ScalaWhitespace._
import fastparse._
import inca.frontend.core.Core
import inca.frontend.extensions._
import inca.frontend.parser._

/** Extension adding "switch" statements to @see CoreParser.
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object SwitchParser extends ParserExtension {

  override def statement: Seq[StatementParser] = Seq(SwitchParser_)

  override def keywords: Seq[String] = Seq("switch")

  object SwitchParser_ extends StatementParser {

    override def parse[_: P]: P[Core.Statement] =
      P(
        P(
          "switch" ~ coreparser.body.rep(sep = "union")
        ).map(v => {
          if (v.size == 1 && v.head.stmts.isEmpty) Switch(Seq.empty)
          else Switch(v)
        })
      )
  }
}
