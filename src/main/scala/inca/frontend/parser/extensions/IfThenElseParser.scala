package inca.frontend.parser.extensions

import fastparse._
import NoWhitespace._
import inca.frontend.core.Core._
import inca.frontend.extensions._
import inca.frontend.parser.ParserUtils._
import inca.frontend.parser._
import inca.frontend.core.Core

/** Extension adding "ifthenelse" statements to @see CoreParser.
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object IfThenElseParser extends ParserExtension {

  override def statement: Seq[StatementParser] = Seq(IfThenElseParser_)

  override def keywords: Seq[String] = Seq("if", "else")

  object IfThenElseParser_ extends StatementParser {

    private def elseif[_: P]: P[ElseIf] =
      P(
        "else" ~ sp ~ "if" ~ sp ~ "(" ~ sp ~ coreparser.exp ~ sp ~ ")" ~ sp ~ coreparser.body
      ).map { case (e, b) => ElseIf(e, b) }

    override def parse[_: P]: P[Core.Statement] =
      P(
        "if" ~ sp ~ "(" ~ sp ~ coreparser.exp ~ sp ~ ")" ~ sp ~ coreparser.body ~
          P(sp ~ elseif ~ sp).rep.? ~
          P(sp ~ "else" ~ coreparser.body).?
      ).map { case (e, b, eifs, el) => IfThenElse(e, b, eifs.getOrElse(Seq.empty), el) }
  }

}
