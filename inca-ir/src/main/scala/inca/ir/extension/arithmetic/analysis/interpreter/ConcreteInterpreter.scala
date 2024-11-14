package inca.ir.extension.arithmetic.analysis.interpreter

import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{ARelationValue, BaseJoinV, CRelationValue, Top, VBool, Value}
import sturdy.effect.{Effect, EffectStack}
import sturdy.effect.failure.Failure
import sturdy.values.Topped
import sturdy.values.integer.{IntegerOps, LiftedIntegerOps, ToppedIntegerOps, given}
import sturdy.values.floating.{FloatOps, LiftedFloatOps, ToppedFloatOps, given}
import sturdy.data.MayJoin
import sturdy.data.MayJoin.NoJoin
import sturdy.values.Topped
import sturdy.values.booleans.BooleanOps
import sturdy.values.ordering.{ConcreteOrderingOps, EqOps, OrderingOps}

case class CIntV(value: Int) extends Value

case class CDoubleV(value: Double) extends Value

private def fromInt(value: Int): Value = value match
    case i => CIntV(i)

private def fromDouble(value: Double): Value = value match
    case d => CDoubleV(d)

private def asInt(v: Value): Int = v match
    case CIntV(i) => i
    case _ => throw IllegalArgumentException(s"Can not convert $v to int")

private def asDouble(v: Value): Double = v match
    case CDoubleV(d) => d
    case _ => throw IllegalArgumentException(s"Can not convert $v to double")

private class CDoubleVOps (using failure: Failure, effects: EffectStack) 
  extends LiftedFloatOps[Double, Value, Double] (asDouble, fromDouble)

private class CIntVOps (using failure: Failure, effects: EffectStack) 
  extends LiftedIntegerOps[Int, Value, Int] (asInt, fromInt)

private class CIntVOrderingOps extends OrderingOps[Value, Boolean]:
    override def lt(v1: Value, v2: Value): Boolean = asInt(v1) < asInt(v2) 
    override def le(v1: Value, v2: Value): Boolean = asInt(v1) <= asInt(v2)

private class CDoubleVOrderingOps extends OrderingOps[Value, Boolean]:
  override def lt(v1: Value, v2: Value): Boolean = asDouble(v1) < asDouble(v2)
  override def le(v1: Value, v2: Value): Boolean = asDouble(v1) <= asDouble(v2)

trait ConcreteInterpreter extends GenericInterpreter[Value, Boolean, CRelationValue[Value], NoJoin]:
  val intOps: IntegerOps[Int, Value] = CIntVOps(using failure, effects)
  val doubleOps: FloatOps[Double, Value] = CDoubleVOps(using failure, effects)
  val intOrderingOps: OrderingOps[Value, Boolean] = CIntVOrderingOps()
  val doubleOrderingOps: OrderingOps[Value, Boolean] = CDoubleVOrderingOps()
