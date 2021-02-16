package inca.frontend_old.extensions.boolOps

import inca.frontend_old.core.tree.Expression
import inca.frontend_old.extensions.boolOps.Trees._
import inca.frontend_old.parser.CoreParser

trait Parser extends CoreParser {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override protected[frontend_old] def infixExp[_: P]: P[Expression] = Chain(andExp, "||", andExp, Or)
  protected[frontend_old] def andExp[_: P]: P[Expression] = Chain(notExp, "&&", notExp, And)
  protected[frontend_old] def notExp[_: P]: P[Expression] = P(("!" ~ super.infixExp).mapWithLoc(Not) | super.infixExp)
}
