package inca.viatra.runtime.aggregate

import org.eclipse.viatra.query.runtime.matchers.psystem.aggregations.IMultisetAggregationOperator

trait MonoAggregation[ST,In,Out] extends Aggregation[In,Out] {
  val name: String
  def init: ST
  def add(st: ST, a: In): ST
  def result(st: ST): Out
  def combine(o1: Out, o2: Out): Out
  def aggregator: IMultisetAggregationOperator[In, _, Out] =
    new AggregatorMono(this)
}