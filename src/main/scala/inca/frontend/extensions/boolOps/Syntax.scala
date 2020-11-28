package inca.frontend.extensions.boolOps

import inca.frontend.core

trait Syntax extends core.Syntax {
  def Not(cond: Expression): Expression
  def And(e1: Expression, e2: Expression): Expression
  def Or(e1: Expression, e2: Expression): Expression
}
