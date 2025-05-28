package inca.ir.extension.aggregate.analysis.interpreter

import inca.ir.RelationBase
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{FiniteAbstractRelation, FiniteAbstractRelationOps, Value}
import inca.ir.extension.arithmetic.ArithmeticAggregationOperator
import inca.ir.extension.aggregate.AggregationOperator
import inca.ir.extension.arithmetic.analysis.interpreter.{IntOps, finiteUpperBound}
import sturdy.data.{MayJoin, WithJoin}
import sturdy.values.booleans.BooleanBranching
import sturdy.values.floating.FloatOps
import sturdy.values.{Powerset, Topped}


trait TerminationAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], FiniteAbstractRelation, Powerset[BaseIRException], WithJoin]:
  val intOps: IntOps[Int, Value]
  val doubleOps: FloatOps[Double, Value]
  val relationOps: FiniteAbstractRelationOps[Powerset[BaseIRException]]
  val branchOpsV: BooleanBranching[Topped[Boolean], Value]

  override lazy val aggregateOps: AggregateOps[Value, FiniteAbstractRelation] = new AggregateOps[Value, FiniteAbstractRelation]:
    override def init(op: AggregationOperator): Value = op match
      case ArithmeticAggregationOperator.MinInt => intOps.integerLit(Int.MaxValue)
      case ArithmeticAggregationOperator.MaxInt => intOps.integerLit(Int.MinValue)
      case ArithmeticAggregationOperator.SumInt => intOps.integerLit(0)
      case ArithmeticAggregationOperator.MinDouble => doubleOps.floatingLit(Double.MaxValue)
      case ArithmeticAggregationOperator.MaxDouble => doubleOps.floatingLit(Double.MinValue)
      case ArithmeticAggregationOperator.SumDouble => doubleOps.floatingLit(0)
      case _ => Value.Top

    override def aggregate(accumulator: Value, value: Value, op: AggregationOperator): Value = op match
      case ArithmeticAggregationOperator.MinInt => intOps.min(accumulator, value)
      case ArithmeticAggregationOperator.MaxInt => intOps.max(accumulator, value)
      case ArithmeticAggregationOperator.SumInt => intOps.add(accumulator, value)
      case ArithmeticAggregationOperator.MinDouble => doubleOps.min(accumulator, value)
      case ArithmeticAggregationOperator.MaxDouble => doubleOps.max(accumulator, value)
      case ArithmeticAggregationOperator.SumDouble => doubleOps.add(accumulator, value)
      case _ => Value.Top

    override def count(rel: RelationBase, rv: FiniteAbstractRelation): Value =
      branchOpsV.boolBranch(relationOps.isFinite(rv)) {
        intOps.integerLit(finiteUpperBound)
      } {
        Value.Top
      }
