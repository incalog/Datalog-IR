package inca.ir.analysis.base.ordering

import inca.ir.analysis.base.values.Value
import sturdy.values.Topped
import sturdy.values.booleans.BooleanOps
import sturdy.values.ordering.EqOps

trait BaseEqOps extends EqOps[Value, Topped[Boolean]]:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case _ => Topped.Top

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case _ => Topped.Top
