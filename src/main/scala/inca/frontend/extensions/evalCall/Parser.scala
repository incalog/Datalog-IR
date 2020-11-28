package inca.frontend.extensions.evalCall

import inca.frontend.parser.CoreParser

trait Parser extends CoreParser {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override val syntax: Syntax
  import syntax._

  override protected[frontend] def atomicExp[_: P]: P[Expression] =
    P("`" ~ evalCore ~ "`" ~ "(" ~ exp.rep(sep = ",") ~ ")").mapWithLoc {
      case (eval, args) => EvalCall(eval, args)
    } | super.atomicExp

}
