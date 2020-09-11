package inca.frontend.parser.extensions

import fastparse._
import NoWhitespace._
import inca.frontend.core.Core._
import inca.frontend.extensions._
import inca.frontend.parser.ParserUtils._
import inca.frontend.parser._

/** Extension adding boolean expressions to @see CoreParser.
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object BoolOpsParser extends ParserExtension {

  override def anchorExpression: Seq[AnchorExpressionParser] = Seq(NotParser)

  override def recursiveExpression: Seq[RecursiveExpressionParser] =
    Seq(AndParser, OrParser)

  object NotParser extends AnchorExpressionParser {
    override def parse[_: P]: P[Exp] =
      P(
        "!" ~ sp ~ coreparser.exp
      ).map(Not)
  }

  object AndParser extends RecursiveExpressionParser {
    override def parse[_: P](e: Exp): P[Exp] =
      P(
        sp ~ "&&" ~ sp ~ coreparser.exp
      ).map(And(e, _))
  }

  object OrParser extends RecursiveExpressionParser {
    override def parse[_: P](e: Exp): P[Exp] =
      P(
        sp ~ "||" ~ sp ~ coreparser.exp
      ).map(Or(e, _))
  }

}
