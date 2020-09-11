package inca.frontend.parser.extensions

import fastparse._
import ScalaWhitespace._
import inca.frontend.core.Core._
import inca.frontend.extensions._
import inca.frontend.parser.ParserUtils._
import inca.frontend.parser._
import inca.frontend.core.Core

/** Extension adding "forallexists" statements to @see CoreParser.
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object ForallExistsParser extends ParserExtension {

  override def statement: Seq[StatementParser] = Seq(ExistsParser, ForallParser)

  override def keywords: Seq[String] = Seq("forall", "exists", "in")

  object ForallParser extends StatementParser {
    override def parse[_: P]: P[Core.Statement] =
      P(
        "forall " ~ coreparser.identifier ~~ " " ~ "in " ~ coreparser.exp ~ coreparser.body
      ).map { case (s, e, b) => Forall(s, e, b) }
  }

  object ExistsParser extends StatementParser {
    override def parse[_: P]: P[Core.Statement] =
      P(
        "exists " ~ coreparser.identifier ~~ " " ~ "in" ~ coreparser.exp ~ coreparser.body
      ).map { case (s, e, b) => Exists(s, e, b) }
  }

}
