package inca.ir.extension.aggregate.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.TypeValue.Top
import inca.ir.analysis.base.values.{TypeRelation, TypeValue}
import inca.ir.extension.aggregate.AggregationOperator
import inca.ir.extension.arithmetic.{ArithmeticAggregationOperator, TInt, TDouble}
import sturdy.data.MayJoin.WithJoin
import sturdy.values.{Powerset, Topped}

trait TypeAbstractInterpreter extends GenericInterpreter[TypeValue, Topped[Boolean], TypeRelation, Powerset[BaseIRException], WithJoin]:
  override val aggregateOps: AggregateOps[TypeValue] = new AggregateOps[TypeValue]:
    override def init(op: AggregationOperator): TypeValue = op match
      case ArithmeticAggregationOperator.MaxInt | ArithmeticAggregationOperator.MinInt => TypeValue.AType(TInt)
      case ArithmeticAggregationOperator.MaxDouble | ArithmeticAggregationOperator.MinDouble => TypeValue.AType(TDouble)
      case _ => Top

    override def aggregate(accumulator: TypeValue, value: TypeValue, op: AggregationOperator): TypeValue =
      accumulator.join(value)