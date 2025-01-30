package inca.ir.analysis.base.values

import sturdy.values.{Finite, MaybeChanged}

trait BaseJoinV:
  def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case _ => Value.Top

trait Meet[V]:
  def meet(lhs: V, rhs: V): V
  def apply(lhs: V, rhs: V): MaybeChanged[V] = MaybeChanged(meet(lhs, rhs), lhs)

trait BaseMeetV extends Meet[Value]:
  def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (Value.Top, _) => rhs
    case (_, Value.Top) => lhs

class FiniteV extends Finite[Value]

trait Value:
  def isConstant: Boolean

object Value:
  case object Top extends Value:
    override def isConstant: Boolean = false
