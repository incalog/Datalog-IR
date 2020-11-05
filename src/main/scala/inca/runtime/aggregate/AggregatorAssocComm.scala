package inca.runtime.aggregate

import java.util.stream

import inca.runtime.aggregate.AggregatorAssocComm.Acc
import org.eclipse.viatra.query.runtime.matchers.psystem.aggregations.IMultisetAggregationOperator

import scala.collection.mutable
import scala.jdk.CollectionConverters._



object AggregatorAssocComm {
  // TODO use a balanced tree with continuously maintained aggregate instead of MultiSet[V]
  class Acc[V]() {
    val vals: mutable.MultiSet[V] = mutable.MultiSet()
    var res: Option[V] = None
  }
}

/** An aggregator for operations that are associative and commutative */
class AggregatorAssocComm[V](val agg: Aggregation[V]) extends IMultisetAggregationOperator[V, Acc[V], V] {


  override def getShortDescription: String = agg.name
  override def getName: String = agg.name

  override def createNeutral(): Acc[V] = new Acc()
  override def isNeutral(acc: Acc[V]): Boolean = acc.vals.isEmpty

  override def update(acc: Acc[V], v: V, isInsertion: Boolean): Acc[V] = {
    if (isInsertion) {
      acc.vals += v
      acc.res = acc.res.map(agg.join(_, v))
    } else {
      acc.vals -= v
      acc.res = None
    }
    acc
  }

  override def getAggregate(acc: Acc[V]): V = acc.res match {
    case Some(value) => value
    case None =>
      val v = acc.vals.foldLeft(agg.init)(agg.join)
      acc.res = Some(v)
      v
  }

  override def aggregateStream(str: stream.Stream[V]): V =
    str.iterator().asScala.foldLeft(agg.init)(agg.join)
}
