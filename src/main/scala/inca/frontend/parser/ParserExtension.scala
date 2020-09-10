package inca.frontend.parser

import fastparse._
import inca.frontend.core.Core._

trait AnchorExpressionParser {
  def parse[_: P]: P[Exp]
}
trait RecursiveExpressionParser {
  def parse[_: P](e: Exp): P[Exp]
}
trait StatementParser {
  def parse[_: P]: P[Statement]
}

/** Interface for parsing language extentions in CoreParser
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
trait ParserExtension {
  private[parser] var coreparser: CoreParser = null // Will be set from CoreParser

  /** This function should return a Sequence of expression parsers,
    * which don't require left hand recursion.
    *
    * @return Sequence of parsers
    */
  def anchorExpression: Seq[AnchorExpressionParser] = Seq.empty

  /**
    * This function should return a sequence of expressions parsers that would
    * require left hand recursion.
    * Therefore these parsers must accept a @see Exp as function argument.
    *
    * @return Sequence of parser functions
    */
  def recursiveExpression: Seq[RecursiveExpressionParser] = Seq.empty

  /**
    * This function should return a sequence of statement parsers.
    *
    * @return Sequence of parser functions with type P[Statement]
    */
  def statement: Seq[StatementParser] = Seq.empty

  /**
    * This function should return a list of used keywords only in this extension.
    *
    * @return Sequence of keywords.
    */
  def keywords: Seq[String] = Seq.empty
}
