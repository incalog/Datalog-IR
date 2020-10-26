package inca.frontend.extensions

import fastparse.ScalaWhitespace._
import fastparse._
import inca.frontend.core.Core.Exp
import inca.frontend.parser.CoreParser

/**
 * Extension adding boolean expressions to CoreParser.
 */
trait BoolOpsParser extends CoreParser {
  override def infixExp[_: P]: P[Exp] = Chain(andExp, "||", andExp, Or)

  def andExp[_: P]: P[Exp] = Chain(notExp, "&&", notExp, And)

  def notExp[_: P]: P[Exp] = P(("!" ~ super.infixExp).map(Not) | super.infixExp)
}
