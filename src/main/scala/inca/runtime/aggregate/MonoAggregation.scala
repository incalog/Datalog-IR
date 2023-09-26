package inca.runtime.aggregate

import org.eclipse.viatra.query.runtime.matchers.psystem.aggregations.IMultisetAggregationOperator

trait MonoAggregation[ST,A,B] extends Aggregation[A,ST] {
  val name: String

  def init: ST
  def add(st: ST, a: A): ST

//  val m: MonoAggregation[ST,A,B]
//  val tmp: ST
//  m.result(tmp)
  def result(st: ST): B

  def aggregator: IMultisetAggregationOperator[A, _, ST] =
    new AggregatorMono(this)
}
