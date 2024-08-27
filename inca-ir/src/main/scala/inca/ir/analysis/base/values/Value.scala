package inca.ir.analysis.base.values

import sturdy.values.{Changed, Join, MaybeChanged, Topped, Unchanged}

trait BaseJoinV:
    def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
        case _ => Top

trait Value
case object Top extends Value