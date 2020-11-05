package inca.runtime.aggregate

import java.util.stream

import org.eclipse.viatra.query.runtime.matchers.psystem.aggregations.AbstractMemorylessAggregationOperator

import scala.jdk.CollectionConverters._

/** An aggregator for operations that are associative, commutative, and invertible */
class AggregatorAssocCommInv[V](val agg: Aggregation[V]) extends AbstractMemorylessAggregationOperator[V, V] {
  override def getShortDescription: String = agg.name
  override def getName: String = agg.name

  override def createNeutral(): V = agg.init
  override def isNeutral(result: V): Boolean = result == agg.init

  override def update(oldResult: V, updateValue: V, isInsertion: Boolean): V =
    if (isInsertion)
      agg.join(oldResult, updateValue)
    else
      agg.unjoin(oldResult, updateValue)

  override def aggregateStream(str: stream.Stream[V]): V =
    str.iterator().asScala.foldLeft(agg.init)(agg.join)
}
