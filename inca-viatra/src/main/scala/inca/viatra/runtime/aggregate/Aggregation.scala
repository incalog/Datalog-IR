package inca.viatra.runtime.aggregate

import org.eclipse.viatra.query.runtime.matchers.psystem.aggregations.IMultisetAggregationOperator

trait Aggregation[In, Out] {
  val name: String

  def aggregator: IMultisetAggregationOperator[In, _, Out]
}