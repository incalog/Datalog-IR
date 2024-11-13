package inca.ir.analysis.base.values

import sturdy.values.{Changed, Finite, Join, MaybeChanged, Topped, Unchanged}

trait BaseJoinV:
  def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case _ => Top

class FiniteV extends Finite[Value]

trait Value

object Value

case object Top extends Value