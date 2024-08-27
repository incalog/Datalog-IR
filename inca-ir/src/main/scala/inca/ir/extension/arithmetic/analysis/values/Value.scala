package inca.ir.extension.arithmetic.analysis.values

import inca.ir.analysis.base.values.{BaseJoinV, Value}
import sturdy.effect.failure.Failure
import sturdy.values.Topped
import sturdy.values.integer.{ LiftedIntegerOps, given }
import sturdy.values.floating.{ LiftedFloatOps, given }

case class IntV(value: Int) extends Value
case class DoubleV(value: Double) extends Value

class IntVOps(using failure: Failure) extends LiftedIntegerOps[Int, IntV, Int](_.value, IntV.apply)
class DoubleVOps(using failure: Failure) extends LiftedFloatOps[Double, DoubleV, Double](_.value, DoubleV.apply)

trait JoinV extends BaseJoinV:
  override def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (IntV(i1), IntV(i2)) if i1 == i2 => lhs
    case (DoubleV(d1), DoubleV(d2)) if d1 == d2 => lhs
    case _ => super.join(lhs, rhs)


