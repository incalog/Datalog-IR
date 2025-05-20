package inca.ir.analysis.base.values

import inca.ir.analysis.base.effect.{BaseIRException, EmptyTable}
import sturdy.effect.except.Except
import sturdy.values.{Join, MaybeChanged}

trait BaseCombineV:
  def combine(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case _ => Value.Top

trait BaseJoinV extends BaseCombineV
trait BaseWidenV extends BaseCombineV

trait Meet[V]:
  def meet(lhs: V, rhs: V): V
  def apply(lhs: V, rhs: V): MaybeChanged[V] = MaybeChanged(meet(lhs, rhs), lhs)

trait BaseMeetV(using except: Except[BaseIRException, ?, ?]) extends Meet[Value]:
  def throwBotException(): Value = except.throws(EmptyTable)

  def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (Value.Top, _) => rhs
    case (_, Value.Top) => lhs
    case _ => throwBotException()

trait RequireMeet[V]:
  val meetV: Meet[V]

trait RequireJoin[V]:
  val joinV: Join[V]

trait Value:
  def isConstant: Boolean

object Value:
  case object Top extends Value:
    override def isConstant: Boolean = false
