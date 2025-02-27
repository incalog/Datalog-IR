package inca.ir.extension.aggregate.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.extension.arithmetic.ArithmeticAggregationOperator
import inca.ir.analysis.base.values.{BaseJoinV, ConcreteRelation, Value}
import inca.ir.extension.aggregate.{AggregationOperator, AggregationOperatorBuiltIn, AggregationOperatorUserDefined}
import inca.ir.extension.arithmetic.analysis.interpreter.{CDoubleV, CIntV}
import sturdy.effect.{Effect, EffectStack}
import sturdy.effect.failure.Failure
import sturdy.values.{Powerset, Topped}
import sturdy.data.MayJoin
import sturdy.data.MayJoin.NoJoin
import sturdy.values.Topped
import sturdy.values.booleans.BooleanOps
import sturdy.values.integer.{ConcreteIntegerOps, IntegerOps, LiftedIntegerOps}
import sturdy.values.ordering.{ConcreteOrderingOps, EqOps, OrderingOps}

private class CAggregateOps(using failure: Failure, effects: EffectStack)
  extends AggregateOps[Value]:
  override def init(op: AggregationOperator): Value = op match
    case ArithmeticAggregationOperator.MinInt => CIntV(Int.MaxValue)
    case ArithmeticAggregationOperator.MaxInt => CIntV(Int.MinValue)
    case ArithmeticAggregationOperator.MinDouble => CDoubleV(Double.MaxValue)
    case ArithmeticAggregationOperator.MaxDouble => CDoubleV(Double.MinValue)
    case _ => throw IllegalStateException("Unsupported aggregation operator")

  override def aggregate(accumulator: Value, value: Value, op: AggregationOperator): Value = (accumulator, value, op) match
      case (CIntV(v1), CIntV(v2), ArithmeticAggregationOperator.MinInt) => CIntV(v1.min(v2))
      case (CIntV(v1), CIntV(v2), ArithmeticAggregationOperator.MaxInt) => CIntV(v1.max(v2))
      case (CIntV(v1), CIntV(v2), ArithmeticAggregationOperator.MinDouble) => CDoubleV(v1.min(v2))
      case (CIntV(v1), CIntV(v2), ArithmeticAggregationOperator.MaxDouble) => CDoubleV(v1.max(v2))


trait ConcreteInterpreter extends GenericInterpreter[Value, Boolean, ConcreteRelation[Value], BaseIRException, NoJoin]:
  override val aggregateOps: AggregateOps[Value] = new CAggregateOps()
