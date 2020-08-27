package inca.runtime.aggregate

import java.util.stream

import org.eclipse.viatra.query.runtime.matchers.psystem.aggregations.AbstractMemorylessAggregationOperator

import scala.jdk.CollectionConverters._

/** An aggregator for operations that are associative, commutative, and invertible */
class AggregatorAssocCommInv[V](val name: String, init: V, join: (V, V) => V, unjoin: (V, V) => V) extends AbstractMemorylessAggregationOperator[V, V] {
  override def getShortDescription: String = name
  override def getName: String = name

  override def createNeutral(): V = init
  override def isNeutral(result: V): Boolean = result == init

  override def update(oldResult: V, updateValue: V, isInsertion: Boolean): V =
    if (isInsertion)
      join(oldResult, updateValue)
    else
      unjoin(oldResult, updateValue)

  override def aggregateStream(str: stream.Stream[V]): V =
    str.iterator().asScala.foldLeft(init)(join)
}
