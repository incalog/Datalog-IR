package inca.ir.extension.aggregate.analysis.interpreter

import inca.ir.RelationBase
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.extension.arithmetic.ArithmeticAggregationOperator
import inca.ir.analysis.base.values.{BaseJoinV, ConcreteRelation, Value}
import inca.ir.extension.aggregate.{AggregationOperator, AggregationOperatorBuiltIn, AggregationOperatorUserDefined}
import inca.ir.extension.arithmetic.analysis.interpreter.{CDoubleV, CIntV, IntOps}
import sturdy.effect.{Effect, EffectStack}
import sturdy.effect.failure.Failure
import sturdy.values.{Powerset, Topped}
import sturdy.data.MayJoin
import sturdy.data.MayJoin.NoJoin
import sturdy.values.Topped
import sturdy.values.booleans.BooleanOps
import sturdy.values.floating.FloatOps
import sturdy.values.integer.{ConcreteIntegerOps, IntegerOps, LiftedIntegerOps}
import sturdy.values.ordering.{ConcreteOrderingOps, EqOps, OrderingOps}

private class CAggregateOps(using failure: Failure, effects: EffectStack, intOps: IntOps[Int, Value], doubleOps: FloatOps[Double, Value])
  extends AggregateOps[Value, ConcreteRelation[Value]]:

  override def init(op: AggregationOperator): Value = op match
    case ArithmeticAggregationOperator.MinInt => intOps.integerLit(Int.MaxValue)
    case ArithmeticAggregationOperator.MaxInt => intOps.integerLit(Int.MinValue)
    case ArithmeticAggregationOperator.SumInt => intOps.integerLit(0)
    case ArithmeticAggregationOperator.MinDouble => doubleOps.floatingLit(Double.MaxValue)
    case ArithmeticAggregationOperator.MaxDouble => doubleOps.floatingLit(Double.MinValue)
    case ArithmeticAggregationOperator.SumDouble => doubleOps.floatingLit(0)
    case _ => failure(UnknownAggregationOperator, s"Unsupported aggregation operator $op")

  override def aggregate(accumulator: Value, value: Value, op: AggregationOperator): Value = op match
    case ArithmeticAggregationOperator.MinInt => intOps.min(accumulator, value)
    case ArithmeticAggregationOperator.MaxInt => intOps.max(accumulator, value)
    case ArithmeticAggregationOperator.SumInt => intOps.add(accumulator, value)
    case ArithmeticAggregationOperator.MinDouble => doubleOps.min(accumulator, value)
    case ArithmeticAggregationOperator.MaxDouble => doubleOps.max(accumulator, value)
    case ArithmeticAggregationOperator.SumDouble => doubleOps.add(accumulator, value)
    case _ => failure(UnknownAggregationOperator, s"Unsupported aggregation operator $op")

  override def count(rel: RelationBase, rv: ConcreteRelation[Value]): Value =
    intOps.integerLit(rv.rows.size)

trait ConcreteInterpreter extends GenericInterpreter[Value, Boolean, ConcreteRelation[Value], BaseIRException, NoJoin]:
  val intOps: IntOps[Int, Value]
  val doubleOps: FloatOps[Double, Value]

  override lazy val aggregateOps: AggregateOps[Value, ConcreteRelation[Value]] = new CAggregateOps(using failure, effects, intOps, doubleOps)
