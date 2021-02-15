package inca.frontend_old.extensions.evalCall

import inca.frontend_old.core

trait Syntax extends core.Syntax {
  def EvalCall(fun: Eval, args: Seq[Expression]): Expression
}
