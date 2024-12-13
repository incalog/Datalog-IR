package inca.ir.analysis.base.values

import sturdy.values.Finite

trait BaseJoinV:
  def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (Bottom, _) => rhs
    case (_, Bottom) => lhs
    case _ => Top

/*trait BaseMeetV:
  def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (Top, _) => rhs
    case (_, Top) => lhs
    case _ => Bottom*/

class FiniteV extends Finite[Value]

trait Value

case object Top extends Value
case object Bottom extends Value
