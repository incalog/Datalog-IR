package inca.ir.extension.aggregate.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.Value.Top
import inca.ir.analysis.base.values.{AType, TypeRelation, Value, joinTypeValue}
import inca.ir.extension.aggregate.AggregationOperator
import inca.ir.extension.arithmetic.{ArithmeticAggregationOperator, TDouble, TInt}
import sturdy.data.MayJoin.WithJoin
import sturdy.values.{Powerset, Topped}

trait TypeAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], TypeRelation, Powerset[BaseIRException], WithJoin]:
  override val aggregateOps: AggregateOps[Value] = new AggregateOps[Value]:
    override def init(op: AggregationOperator): Value = op match
      case ArithmeticAggregationOperator.MaxInt | ArithmeticAggregationOperator.MinInt => AType(TInt)
      case ArithmeticAggregationOperator.MaxDouble | ArithmeticAggregationOperator.MinDouble => AType(TDouble)
      case _ => Top

    override def aggregate(accumulator: Value, value: Value, op: AggregationOperator): Value =
      joinTypeValue(accumulator, value)