package inca.ir.analysis.base.values

import inca.ir.analysis.base.effect.Failure.TypeError
import sturdy.effect.failure.Failure
import sturdy.values.{Changed, Join, MaybeChanged, Topped, Unchanged}
import sturdy.values.ordering.EqOps
import sturdy.values.booleans.{BooleanOps, LiftedBooleanOps}

// Implicits
import sturdy.values.booleans.given

case class VBool(b: Topped[Boolean]):

  def join(that: VBool): VBool = (this, that) match
    case (VBool(Topped.Actual(b1)), VBool(Topped.Actual(b2))) => if b1 == b2 then this else VBool.Top
    case _ => VBool.Top

object VBool:
  val False: VBool = VBool(Topped.Actual(false))
  val True: VBool = VBool(Topped.Actual(true))
  val Top: VBool = VBool(Topped.Top)

given JoinVBool: Join[VBool] with
  override def apply(v1: VBool, v2: VBool): MaybeChanged[VBool] = if v1 == v2 then Unchanged(v1) else Changed(v1.join(v2))

class VBoolOps(using failure: Failure) extends LiftedBooleanOps[VBool, Topped[Boolean]](_.b, VBool.apply)
