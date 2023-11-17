package inca.viatra.runtime.aggregate

import org.eclipse.viatra.query.runtime.matchers.psystem.aggregations.IMultisetAggregationOperator

import java.util.stream
import scala.jdk.CollectionConverters.*

/** An aggregator for operations that are associative and commutative */
class AggregatorAssocComm[V](val agg: JoinAggregation[V]) extends IMultisetAggregationOperator[V, AugmentedAVLTree[V], V]:

  override def getShortDescription: String = agg.name
  override def getName: String = agg.name

  override def createNeutral(): AugmentedAVLTree[V] = new AugmentedAVLTree[V](agg.join)(agg.ord)
  override def isNeutral(acc: AugmentedAVLTree[V]): Boolean = acc.root == null

  override def update(acc: AugmentedAVLTree[V], v: V, isInsertion: Boolean): AugmentedAVLTree[V] = {
    if (isInsertion)
      acc.insert(v)
    else
      acc.remove(v)
    acc
  }

  override def getAggregate(acc: AugmentedAVLTree[V]): V = {
    if (acc.root == null) null.asInstanceOf[V]
    else acc.root.computedValue
  }

  override def combine(left: V, right: AugmentedAVLTree[V]): V = {
    if (left == null && right == null) null.asInstanceOf[V]
    else if (left == null)
      getAggregate(right)
    else if (right == null)
      left
    else {
      val rightAgg = getAggregate(right)
      if (rightAgg == null)
        left
      else agg.join(left, rightAgg)
    }
  }

  override def contains(value: V, accumulator: AugmentedAVLTree[V]): Boolean = accumulator.find(value) != null

  override def aggregateStream(str: stream.Stream[V]): V =
    str.iterator().asScala.foldLeft(agg.init)(agg.join)
