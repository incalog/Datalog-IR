package inca.ir.extension.arithmetic.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{ARelationValue, BaseJoinV, Top, Value}
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

case class ConstantIntV(value: Int) extends Value

case class ConstantDoubleV(value: Double) extends Value

trait ConstantEqOps(using boolOps: BooleanOps[Topped[Boolean]]) extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (ConstantIntV(i1), ConstantIntV(i2)) => boolOps.boolLit(i1 == i2)
    case (ConstantDoubleV(d1), ConstantDoubleV(d2)) => boolOps.boolLit(d1 == d2)
    case _ => super.equ(v1, v2)

private def constantIntFromToppedInt(value: Topped[Int]): Value = value match
    case Topped.Top => Top
    case Topped.Actual(i) => ConstantIntV(i)

private def constantDoubleFromToppedDouble(value: Topped[Double]): Value = value match
    case Topped.Top => Top
    case Topped.Actual(d) => ConstantDoubleV(d)

private def toppedIntAsConstantInt(v: Value): Topped[Int] = v match
    case ConstantIntV(i) => Topped.Actual(i)
    case Top => Topped.Top
    case _ => throw IllegalArgumentException(s"Can not convert $v to int")

private def toppedDoubleAsConstantDouble(v: Value): Topped[Double] = v match
    case ConstantDoubleV(d) => Topped.Actual(d)
    case Top => Topped.Top
    case _ => throw IllegalArgumentException(s"Can not convert $v to double")

private class ConstantDoubleVOps (using failure: Failure, effects: EffectStack) 
  extends LiftedFloatOps[Double, Value, Topped[Double]] (toppedDoubleAsConstantDouble, constantDoubleFromToppedDouble) (
    using ToppedFloatOps[Double, Double] (using implicitly) //  failure and effects are not needed... why?
  )

private class ConstantIntVOps (using failure: Failure, effects: EffectStack) 
  extends LiftedIntegerOps[Int, Value, Topped[Int]] (toppedIntAsConstantInt, constantIntFromToppedInt) (
    using ToppedIntegerOps[Int, Int] (using implicitly, failure, effects)
  )

private class ConstantIntVOrderingOps 
  extends LiftedOrderingOps[Value, Topped[Boolean], Topped[Int], Topped[Boolean]](toppedIntAsConstantInt, identity)

private class ConstantDoubleVOrderingOps 
  extends LiftedOrderingOps[Value, Topped[Boolean], Topped[Double], Topped[Boolean]](toppedDoubleAsConstantDouble, identity)

trait ConstantJoinV extends BaseJoinV:
  override def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantIntV(i1), ConstantIntV(i2)) if i1 == i2 => lhs
    case (ConstantDoubleV(d1), ConstantDoubleV(d2)) if d1 == d2 => lhs
    case _ => super.join(lhs, rhs)

// Constant Analysis
trait ConstantAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], ARelationValue[Value], Powerset[BaseIRException], WithJoin]:
  val intOps: IntegerOps[Int, Value] = ConstantIntVOps(using failure, effects)
  val doubleOps: FloatOps[Double, Value] = ConstantDoubleVOps(using failure, effects)
  val intOrderingOps: OrderingOps[Value, Topped[Boolean]] = ConstantIntVOrderingOps()
  val doubleOrderingOps: OrderingOps[Value, Topped[Boolean]] = ConstantDoubleVOrderingOps()
