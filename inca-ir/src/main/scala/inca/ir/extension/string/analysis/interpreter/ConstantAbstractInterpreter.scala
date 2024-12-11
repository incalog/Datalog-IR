package inca.ir.extension.string.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, ConstantRelation, Top, Value}
import sturdy.effect.{Effect, EffectStack}
import sturdy.effect.failure.Failure
import sturdy.values.{Powerset, Topped}
import sturdy.values.floating.{FloatOps, LiftedFloatOps, ToppedFloatOps, given}
import sturdy.data.MayJoin
import sturdy.values.Topped
import sturdy.values.booleans.BooleanOps
import sturdy.values.integer.{ConcreteIntegerOps, IntegerOps, LiftedIntegerOps, ToppedIntegerOps}
import sturdy.values.ordering.{LiftedOrderingOps, OrderingOps, ToppedCertainOrderingOps}
import sturdy.data.{MakeJoined, WithJoin}
import sturdy.values.integer.given_OrderingOps_Int_Boolean

case class ConstantStringV(value: String) extends Value:
  override def toString: String = value

trait ConstantEqOps extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (ConstantStringV(s1), ConstantStringV(s2)) => Topped.Actual(s1 == s2)
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (ConstantStringV(s1), ConstantStringV(s2)) => Topped.Actual(s1 != s2)
    case _ => super.neq(v1, v2)

trait ConstantJoinV extends BaseJoinV:
  override def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantStringV(s1), ConstantStringV(s2)) if s1 == s2 => lhs
    case _ => super.join(lhs, rhs)

/*trait ConstantMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantStringV(s1), ConstantStringV(s2)) if s1 == s2 => lhs
    case _ => super.meet(lhs, rhs)*/

class ConstantStringVOps(using failure: Failure) extends StringOps[Value]:
  override def stringLit(s: String): Value = ConstantStringV(s)

  override def toString(v: Value): Value = ConstantStringV(v.toString)

  override def concat(v1: Value, v2: Value): Value = (v1, v2) match
    case (CStringV(s1), CStringV(s2)) => ConstantStringV(s1 ++ s2)
    case _ => failure(InvalidStringConcat, s"Can not concat non-string values $v1 and $v2")


trait ConstantAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], ConstantRelation, Powerset[BaseIRException], WithJoin]:
  val stringOps: StringOps[Value] = ConstantStringVOps(using failure)
