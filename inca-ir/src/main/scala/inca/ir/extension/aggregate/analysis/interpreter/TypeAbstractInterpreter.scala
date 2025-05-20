package inca.ir.extension.aggregate.analysis.interpreter

import inca.ir.RelationBase
import inca.ir.analysis.TypeValue
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.Value.Top
import inca.ir.analysis.base.values.{AbstractRelation, Value}
import inca.ir.extension.aggregate.AggregationOperator
import inca.ir.extension.arithmetic.{ArithmeticAggregationOperator, TDouble, TInt}
import sturdy.data.MayJoin.WithJoin
import sturdy.values.{Powerset, Topped}

trait TypeAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]:
  override lazy val aggregateOps: AggregateOps[Value, AbstractRelation] = new AggregateOps[Value, AbstractRelation]:
    override def init(op: AggregationOperator): Value = op match
      case ArithmeticAggregationOperator.MaxInt | ArithmeticAggregationOperator.MinInt => TypeValue(TInt)
      case ArithmeticAggregationOperator.MaxDouble | ArithmeticAggregationOperator.MinDouble => TypeValue(TDouble)
      case _ => Top

    override def aggregate(accumulator: Value, value: Value, op: AggregationOperator): Value =
      mayJoinV.j(accumulator, value).get

    override def count(rel: RelationBase, rv: AbstractRelation): Value = TypeValue(TInt)
