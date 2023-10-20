package inca.viatra.runtime.aggregate

import org.eclipse.viatra.query.runtime.matchers.psystem.aggregations.IMultisetAggregationOperator

trait Aggregation[V]:
  val name: String

  def init: V
  def join(v1: V, v2: V): V

  // induce ordering based on join operation
  // can be redefined for better efficiency
  val ord: Ordering[V] = (x: V, y: V) =>
    if (x == y) 0
    else if (join(x, y) == x) -1
    else 1

  val isAssociative: Boolean = false
  val isCommutative: Boolean = false

  def hasUnjoin: Boolean = false
  def unjoin(v1: V, v2: V): V = throw new UnsupportedOperationException

  def aggregator: IMultisetAggregationOperator[V, _, V] =
    if (isAssociative && isCommutative && hasUnjoin)
      new AggregatorAssocCommInv[V](this)
    else if (isAssociative && isCommutative)
      new AggregatorAssocComm[V](this)
    else
      throw new UnsupportedOperationException(s"Cannot create aggregator for associative=$isAssociative, commutative=$isCommutative, hasUnjoin=$hasUnjoin")
