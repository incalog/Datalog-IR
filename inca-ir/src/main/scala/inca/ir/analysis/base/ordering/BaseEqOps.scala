package inca.ir.analysis.base.ordering

import inca.ir.analysis.base.values.Value
import sturdy.values.Topped
import sturdy.values.Topped.Top
import sturdy.values.booleans.BooleanOps
import sturdy.values.ordering.EqOps

trait BaseEqOps(using boolOps: BooleanOps[Topped[Boolean]]) extends EqOps[Value, Topped[Boolean]]:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = Top

  override def neq(v1: Value, v2: Value): Topped[Boolean] = boolOps.not(equ(v1, v2))
