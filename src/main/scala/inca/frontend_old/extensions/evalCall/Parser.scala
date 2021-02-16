package inca.frontend_old.extensions.evalCall

import inca.frontend_old.core.tree.Expression
import inca.frontend_old.extensions.evalCall.Trees.EvalCall
import inca.frontend_old.parser.CoreParser

trait Parser extends CoreParser {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override protected[frontend_old] def atomicExp[_: P]: P[Expression] =
    P("`" ~ evalCore ~ "`" ~ "(" ~ exp.rep(sep = ",") ~ ")").mapWithLoc {
      case (eval, args) => EvalCall(eval, args)
    } | super.atomicExp

}
