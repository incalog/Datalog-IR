package inca.frontend.constraint.extensions.evalCall

import inca.frontend.constraint.core.Expression
import inca.frontend.constraint.extensions.evalCall.Trees.EvalCall
import inca.frontend.constraint.parser.CoreParser

trait Parser extends CoreParser {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override protected[frontend] def atomicExp[_: P]: P[Expression] =
    P("`" ~ evalCore ~ "`" ~ "(" ~ exp.rep(sep = ",") ~ ")").mapWithLoc {
      case (eval, args) => EvalCall(eval, args)
    } | super.atomicExp

}
