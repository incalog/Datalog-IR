package inca.frontend.constraint.extensions.boolOps

import inca.frontend.constraint.core.tree.Expression
import inca.frontend.constraint.extensions.boolOps.Trees._
import inca.frontend.constraint.parser.CoreParser

trait Parser extends CoreParser {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override protected[frontend] def infixExp[_: P]: P[Expression] = Chain(andExp, "||", andExp, Or)
  protected[frontend] def andExp[_: P]: P[Expression] = Chain(notExp, "&&", notExp, And)
  protected[frontend] def notExp[_: P]: P[Expression] = P(("!" ~ super.infixExp).mapWithLoc(Not) | super.infixExp)
}
