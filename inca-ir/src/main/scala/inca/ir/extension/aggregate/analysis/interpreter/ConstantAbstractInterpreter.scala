package inca.ir.extension.aggregate.analysis.interpreter

import inca.ir.RelationBase
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{AbstractRelation, BaseJoinV, Value}
import inca.ir.extension.aggregate.{AggregationOperator, AggregationOperatorBuiltIn, AggregationOperatorUserDefined}
import sturdy.effect.{Effect, EffectStack}
import sturdy.effect.failure.Failure
import sturdy.values.{Powerset, Topped}
import sturdy.values.floating.{FloatOps, LiftedFloatOps, ToppedFloatOps, given}
import sturdy.data.MayJoin
import sturdy.values.Topped
import sturdy.values.booleans.BooleanOps
import sturdy.values.integer.{ConcreteIntegerOps, IntegerOps, LiftedIntegerOps, ToppedIntegerOps}
import sturdy.values.ordering.{LiftedOrderingOps, OrderingOps, ToppedCertainOrderingOps}
import sturdy.data.{MakeJoined, WithJoin}
import sturdy.effect.except.Except
import sturdy.values.integer.given_OrderingOps_Int_Boolean


trait ConstantAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]:
  override lazy val aggregateOps: AggregateOps[Value, AbstractRelation] = new AggregateOps[Value, AbstractRelation]:
    override def init(op: AggregationOperator): Value =
      // we could make this more precise, but there is really no point. Once we aggregate more than the initial value
      // we don't know anything anyway.
      Value.Top

    override def aggregate(accumulator: Value, value: Value, op: AggregationOperator): Value =
      // Again, we could make this more precise by handling different aggregation operators, but the problem remains
      // the same as above.
      Value.Top

    override def count(rel: RelationBase, rv: AbstractRelation): Value =
      Value.Top