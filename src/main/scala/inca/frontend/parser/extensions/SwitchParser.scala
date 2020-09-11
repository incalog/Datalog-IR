package inca.frontend.parser.extensions

import fastparse._
import NoWhitespace._
import inca.frontend.core.Core._
import inca.frontend.extensions._
import inca.frontend.parser.ParserUtils._
import inca.frontend.parser._
import inca.frontend.core.Core

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
          sp ~ "switch" ~ P(sp_nl ~ coreparser.body ~ sp_nl)
            .rep(sep = P(sp ~ "union" ~ sp))
        ).map(v => {
          if (v.size == 1 && v(0).stmts.isEmpty) Switch(Seq.empty)
          else Switch(v)
        })
      )
  }
}
