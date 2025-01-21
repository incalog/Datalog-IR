package inca.ir.extension.arithmetic.analysis.interpreter

import inca.ir.analysis.base.effect.{AtomFailed, BaseIRException}
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, ConstantRelation, Value}
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
import sturdy.effect.except.Except
import sturdy.values.integer.given_OrderingOps_Int_Boolean

case class ConstantIntV(value: Int) extends Value:
  override def toString: String = value.toString
  override def isConstant: Boolean = true

case class ConstantDoubleV(value: Double) extends Value:
  override def toString: String = value.toString
  override def isConstant: Boolean = true

trait ConstantEqOps extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (ConstantIntV(i1), ConstantIntV(i2)) => Topped.Actual(i1 == i2)
    case (ConstantDoubleV(d1), ConstantDoubleV(d2)) => Topped.Actual(d1 == d2)
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (ConstantIntV(i1), ConstantIntV(i2)) => Topped.Actual(i1 != i2)
    case (ConstantDoubleV(d1), ConstantDoubleV(d2)) => Topped.Actual(d1 != d2)
    case _ => super.neq(v1, v2)

private def constantIntFromToppedInt(value: Topped[Int]): Value = value match
    case Topped.Top => Value.Top
    case Topped.Actual(i) => ConstantIntV(i)

private def constantDoubleFromToppedDouble(value: Topped[Double]): Value = value match
    case Topped.Top => Value.Top
    case Topped.Actual(d) => ConstantDoubleV(d)

private def toppedIntAsConstantInt(v: Value)(using except: Except[BaseIRException, ?, ?]): Topped[Int] = v match
    case ConstantIntV(i) => Topped.Actual(i)
    case Value.Top => Topped.Top
    case Value.Bottom => except.throws(AtomFailed("Can not compare with bottom"))
    case _ => throw IllegalArgumentException(s"Can not convert $v to int")

private def toppedDoubleAsConstantDouble(v: Value)(using except: Except[BaseIRException, ?, ?]): Topped[Double] = v match
    case ConstantDoubleV(d) => Topped.Actual(d)
    case Value.Top => Topped.Top
    case Value.Bottom => except.throws(AtomFailed("Can not compare with bottom"))
    case _ => throw IllegalArgumentException(s"Can not convert $v to double")

private class ConstantDoubleVOps (using failure: Failure, effects: EffectStack, except: Except[BaseIRException, ?, ?])
  extends LiftedFloatOps[Double, Value, Topped[Double]] (toppedDoubleAsConstantDouble, constantDoubleFromToppedDouble) (
    using ToppedFloatOps[Double, Double] (using implicitly) //  failure and effects are not needed... why?
  )

private class ConstantIntVOps (using failure: Failure, effects: EffectStack, except: Except[BaseIRException, ?, ?])
  extends LiftedIntegerOps[Int, Value, Topped[Int]](toppedIntAsConstantInt, constantIntFromToppedInt) (
    using ToppedIntegerOps[Int, Int](using implicitly, failure, effects)
  )

private class ConstantIntVOrderingOps(using except: Except[BaseIRException, ?, ?])
  extends LiftedOrderingOps[Value, Topped[Boolean], Topped[Int], Topped[Boolean]](toppedIntAsConstantInt, identity)

private class ConstantDoubleVOrderingOps(using except: Except[BaseIRException, ?, ?])
  extends LiftedOrderingOps[Value, Topped[Boolean], Topped[Double], Topped[Boolean]](toppedDoubleAsConstantDouble, identity)

trait ConstantJoinV extends BaseJoinV:
  override def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantIntV(i1), ConstantIntV(i2)) if i1 == i2 => lhs
    case (ConstantDoubleV(d1), ConstantDoubleV(d2)) if d1 == d2 => lhs
    case _ => super.join(lhs, rhs)

/*trait ConstantMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantIntV(i1), ConstantIntV(i2)) if i1 == i2 => lhs
    case (ConstantDoubleV(d1), ConstantDoubleV(d2)) if d1 == d2 => lhs
    case _ => super.meet(lhs, rhs)*/

trait ConstantAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], ConstantRelation, Powerset[BaseIRException], WithJoin]:
  val intOps: IntegerOps[Int, Value] = ConstantIntVOps(using failure, effects, except)
  val doubleOps: FloatOps[Double, Value] = ConstantDoubleVOps(using failure, effects, except)
  val intOrderingOps: OrderingOps[Value, Topped[Boolean]] = ConstantIntVOrderingOps(using except)
  val doubleOrderingOps: OrderingOps[Value, Topped[Boolean]] = ConstantDoubleVOrderingOps(using except)
