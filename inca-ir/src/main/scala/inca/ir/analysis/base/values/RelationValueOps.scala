package inca.ir.analysis.base.values

import inca.ir.Name
import inca.ir.analysis.RelationOps
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.effect.Failure.*
import sturdy.effect.EffectStack
import sturdy.effect.failure.Failure
import sturdy.values.Join
import sturdy.values.booleans.BooleanOps
import sturdy.values.ordering.EqOps

class RelationValueOps[C, V, B](using effects: EffectStack, boolOps: BooleanOps[B], eqOps: EqOps[V, B], failure: Failure)
  extends RelationOps[C, V, B, RelationValue[C, V]](using boolOps):

  type RV = RelationValue[C, V]

  ???


