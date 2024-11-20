package inca.ir.extension.arithmetic.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{ARelationValue, Top, VBool, Value}
import inca.ir.analysis.base.values.{BaseJoinV, Top, Value}
import sturdy.effect.{Effect, EffectStack}
import sturdy.effect.failure.Failure
import sturdy.values.Topped
import sturdy.values.integer.{IntegerOps, LiftedIntegerOps, ToppedIntegerOps, given}
import sturdy.values.floating.{FloatOps, LiftedFloatOps, ToppedFloatOps, given}
import sturdy.data.MayJoin
import sturdy.values.Topped
import sturdy.values.booleans.BooleanOps
import sturdy.values.ordering.{LiftedOrderingOps, OrderingOps, ToppedCertainOrderingOps}

case class ConstantIntV(value: Int) extends Value

case class ConstantDoubleV(value: Double) extends Value

trait ConstantEqOps(using boolOps: BooleanOps[VBool]) extends BaseEqOps:
  override def equ(v1: Value, v2: Value): VBool = (v1, v2) match
    case (ConstantIntV(i1), ConstantIntV(i2)) => boolOps.boolLit(i1 == i2)
    case (ConstantDoubleV(d1), ConstantDoubleV(d2)) => boolOps.boolLit(d1 == d2)
    case _ => super.equ(v1, v2)

// Note: We could also introduce different abstractions here, such as intervals
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
  extends LiftedOrderingOps[Value, VBool, Topped[Int], Topped[Boolean]](toppedIntAsConstantInt, VBool.apply)

private class ConstantDoubleVOrderingOps 
  extends LiftedOrderingOps[Value, VBool, Topped[Double], Topped[Boolean]](toppedDoubleAsConstantDouble, VBool.apply)

trait ConstantJoinV extends BaseJoinV:
  override def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantIntV(i1), ConstantIntV(i2)) if i1 == i2 => lhs
    case (ConstantDoubleV(d1), ConstantDoubleV(d2)) if d1 == d2 => lhs
    case _ => super.join(lhs, rhs)

// Constant Analysis
trait ConstantAbstractInterpreter[J[_] <: MayJoin[?]] extends GenericInterpreter[Value, VBool, ARelationValue[Value], Unit, J]:
  val intOps: IntegerOps[Int, Value] = ConstantIntVOps(using failure, effects)
  val doubleOps: FloatOps[Double, Value] = ConstantDoubleVOps(using failure, effects)
  val intOrderingOps: OrderingOps[Value, VBool] = ConstantIntVOrderingOps()
  val doubleOrderingOps: OrderingOps[Value, VBool] = ConstantDoubleVOrderingOps()
