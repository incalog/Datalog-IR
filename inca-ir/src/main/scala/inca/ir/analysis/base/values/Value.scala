package inca.ir.analysis.base.values

import sturdy.values.Finite

trait BaseJoinV:
  def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (Value.Bottom, _) => rhs
    case (_, Value.Bottom) => lhs
    case _ => Value.Top

/*trait BaseMeetV:
  def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (Top, _) => rhs
    case (_, Top) => lhs
    case _ => Bottom*/

class FiniteV extends Finite[Value]

trait Value:
  def isConstant: Boolean

object Value:
  case object Top extends Value:
    override def isConstant: Boolean = false
  case object Bottom extends Value:
    override def isConstant: Boolean = true
