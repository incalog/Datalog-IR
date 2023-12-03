package inca.viatra.runtime.aggregate

import org.eclipse.viatra.query.runtime.matchers.psystem.aggregations.IMultisetAggregationOperator

trait MonoAggregation[ST,A] extends Aggregation[A,ST] {
  val name: String
  def init: ST
  def add(st: ST, a: A): ST
  def aggregator: IMultisetAggregationOperator[A, _, ST] =
    new AggregatorMono(this)
}