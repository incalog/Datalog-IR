package inca.ir.analysis.base.values

import sturdy.values.Finite

trait BaseJoinV:
  def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case _ => Top

class FiniteV extends Finite[Value]

trait Value

case object Top extends Value
