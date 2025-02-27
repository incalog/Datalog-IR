package inca.ir.analysis.base.values

import inca.ir.analysis.base.effect.{BaseIRException, EmptyTable}
import sturdy.effect.except.Except
import sturdy.values.{Finite, MaybeChanged}

trait BaseJoinV:
  def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case _ => Value.Top

trait Meet[V]:
  def meet(lhs: V, rhs: V): V
  def apply(lhs: V, rhs: V): MaybeChanged[V] = MaybeChanged(meet(lhs, rhs), lhs)

trait BaseMeetV(using except: Except[BaseIRException, ?, ?]) extends Meet[Value]:
  def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (Value.Top, _) => rhs
    case (_, Value.Top) => lhs
    case _ => except.throws(EmptyTable)

class FiniteV extends Finite[Value]

trait Value:
  def isConstant: Boolean
  def isActual: Boolean = true

object Value:
  case object Top extends Value:
    override def isConstant: Boolean = false
