package inca.ir.extension.arithmetic.analysis.values

import inca.ir.analysis.base.values.{BaseJoinV, Value}
import sturdy.effect.failure.Failure
import sturdy.values.Topped
import sturdy.values.integer.{ LiftedIntegerOps, given }
import sturdy.values.floating.{ LiftedFloatOps, given }

// Note: We could also introduce different abstractions here, such as intervals
extension (v: Value)
  def asInt: Int = v match
    case IntV(i) => i
    case _ => throw IllegalArgumentException(s"Can not convert $v to int")

  def asDouble: Double = v match
    case DoubleV(d) => d
    case _ => throw IllegalArgumentException(s"Can not convert $v to double")


case class IntV(value: Int) extends Value
case class DoubleV(value: Double) extends Value

class IntVOps(using failure: Failure) extends LiftedIntegerOps[Int, Value, Int](_.asInt, IntV.apply)
class DoubleVOps(using failure: Failure) extends LiftedFloatOps[Double, Value, Double](_.asDouble, DoubleV.apply)

trait JoinV extends BaseJoinV:
  override def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (IntV(i1), IntV(i2)) if i1 == i2 => lhs
    case (DoubleV(d1), DoubleV(d2)) if d1 == d2 => lhs
    case _ => super.join(lhs, rhs)


