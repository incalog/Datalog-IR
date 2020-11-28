package inca.frontend.extensions.evalCall

import inca.frontend.core

trait Syntax extends core.Syntax {
  def EvalCall(fun: Eval, args: Seq[Expression]): Expression
}
