package inca.ir.extension.aggregate.analysis.interpreter

import inca.ir.RelationBase
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{AbstractRelation, Value}
import inca.ir.extension.arithmetic.ArithmeticAggregationOperator
import inca.ir.extension.aggregate.AggregationOperator
import inca.ir.extension.arithmetic.analysis.interpreter.IntOps
import sturdy.data.{MayJoin, WithJoin}
import sturdy.values.floating.FloatOps
import sturdy.values.{Powerset, Topped}


trait TerminationAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]:
  val intOps: IntOps[Int, Value]
  val doubleOps: FloatOps[Double, Value]

  override lazy val aggregateOps: AggregateOps[Value, AbstractRelation] = new AggregateOps[Value, AbstractRelation]:
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

    // TODO: How do we do this?
    override def count(rel: RelationBase, rv: AbstractRelation): Value =
      Value.Top

      // This is wrong, because:
      // R(x: String, y: Int) :- x == "A", y == 0.
      // R(x: String, y: Int) :- R(x, z), y == z + 1.
      //
      // Q(x: String) :- R(x, _).
      // With the method above Q would be finite, even though R is infinite

      // If any of the values in the relation is top, the relation might be non-terminating
      /*val filtered = relationOps.filter(rv) { row =>
        val containsTopValue = row.contains(Value.Top)
        Topped.Actual(containsTopValue)
      }
      if (relationOps.isEmpty(filtered) == Topped.Actual(true))
        // we know the relation is finite
        intOps.integerLit(5000) // TODO: Replace this with a symbolic number
      else
        // Possibly non-terminating, that is we can not count
        throw IllegalStateException("Count is infinite")
        //Value.Top*/
