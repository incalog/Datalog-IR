package inca.ir.analysis.base.ordering

import inca.ir.analysis.base.values.{Bottom, Value}
import sturdy.values.Topped
import sturdy.values.booleans.BooleanOps
import sturdy.values.ordering.EqOps

trait BaseEqOps extends EqOps[Value, Topped[Boolean]]:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (Bottom, Bottom) => Topped.Actual(true)
    case _ => Topped.Top

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (Bottom, Bottom) => Topped.Actual(false)
    case _ => Topped.Top
