package inca.ir.extension.arithmetic.analysis.values

import inca.ir.analysis.base.values.{BaseJoinV, Top, Value}
import sturdy.effect.{Effect, EffectStack}
import sturdy.effect.failure.Failure
import sturdy.values.Topped
import sturdy.values.integer.{LiftedIntegerOps, ToppedIntegerOps, given}
import sturdy.values.floating.{LiftedFloatOps, ToppedFloatOps, given}

// Note: We could also introduce different abstractions here, such as intervals
extension (v: Value.type)
  def fromInt(value: Topped[Int]): Value = value match
    case Topped.Top => Top
    case Topped.Actual(i) => IntV(i)

  def fromDouble(value: Topped[Double]): Value = value match
    case Topped.Top => Top
    case Topped.Actual(d) => DoubleV(d)

extension (v: Value)
  def asInt: Topped[Int] = v match
    case IntV(i) => Topped.Actual(i)
    case Top => Topped.Top
    case _ => throw IllegalArgumentException(s"Can not convert $v to int")

  def asDouble: Topped[Double] = v match
    case DoubleV(d) => Topped.Actual(d)
    case Top => Topped.Top
    case _ => throw IllegalArgumentException(s"Can not convert $v to double")


case class IntV(value: Int) extends Value
case class DoubleV(value: Double) extends Value

class DoubleVOps(using failure: Failure, effects: EffectStack) extends LiftedFloatOps[Double, Value, Topped[Double]](_.asDouble, Value.fromDouble)(
  using ToppedFloatOps[Double, Double](using implicitly) //  failure and effects are not needed... why?
)
class IntVOps(using failure: Failure, effects: EffectStack) extends LiftedIntegerOps[Int, Value, Topped[Int]](_.asInt, Value.fromInt)(
  using ToppedIntegerOps[Int, Int](using implicitly, failure, effects)
)

trait JoinV extends BaseJoinV:
  override def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (IntV(i1), IntV(i2)) if i1 == i2 => lhs
    case (DoubleV(d1), DoubleV(d2)) if d1 == d2 => lhs
    case _ => super.join(lhs, rhs)


