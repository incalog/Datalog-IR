package inca.ir.extension.arithmetic.analysis.ordering

import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{VBool, VBoolOps, Value}
import inca.ir.analysis.base.values.VBool
import inca.ir.extension.arithmetic.analysis.values.{DoubleV, IntV}
import sturdy.values.Topped
import sturdy.values.booleans.BooleanOps
import sturdy.values.ordering.{EqOps, LiftedOrderingOps}

trait EqOps(using boolOps: VBoolOps) extends BaseEqOps:
  override def equ(v1: Value, v2: Value): VBool = (v1, v2) match
    case (IntV(i1), IntV(i2)) => boolOps.boolLit(i1 == i2)
    case (DoubleV(d1), DoubleV(d2)) => boolOps.boolLit(d1 == d2)
    case _ => super.equ(v1, v2)