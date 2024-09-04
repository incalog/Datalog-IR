package inca.ir.analysis.base.ordering

import inca.ir.analysis.base.values.{VBool, VBoolOps, Value}
import sturdy.values.Topped
import sturdy.values.booleans.BooleanOps
import sturdy.values.ordering.EqOps

trait BaseEqOps(using boolOps: BooleanOps[VBool]) extends EqOps[Value, VBool]:
  override def equ(v1: Value, v2: Value): VBool = VBool.Top
  override def neq(v1: Value, v2: Value): VBool = boolOps.not(equ(v1, v2))
