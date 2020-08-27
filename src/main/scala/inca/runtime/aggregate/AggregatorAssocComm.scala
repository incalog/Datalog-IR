package inca.runtime.aggregate

import java.util.stream

import inca.runtime.aggregate.AggregatorAssocComm.Acc
import org.eclipse.viatra.query.runtime.matchers.psystem.aggregations.IMultisetAggregationOperator

import scala.collection.immutable.MultiSet
import scala.jdk.CollectionConverters._



object AggregatorAssocComm {
  // TODO use a balanced tree with continuously maintained aggregate instead of MultiSet[V]
  class Acc[V](val vs: MultiSet[V], var res: V)
}

/** An aggregator for operations that are associative and commutative */
class AggregatorAssocComm[V](val name: String, init: V, join: (V, V) => V) extends IMultisetAggregationOperator[V, Acc[V], V] {


  override def getShortDescription: String = name
  override def getName: String = name

  override def createNeutral(): Acc[V] = new Acc(MultiSet(), init)
  override def isNeutral(acc: Acc[V]): Boolean = acc.vs.isEmpty

  override def update(acc: Acc[V], v: V, isInsertion: Boolean): Acc[V] =
    if (isInsertion) {
      val vs = acc.vs + v
      if (acc.res == null)
        new Acc(vs, acc.res)
      else
        new Acc(vs, join(acc.res, v))
    } else {
      val vs = acc.vs - v
      new Acc(vs, null.asInstanceOf[V])
    }

  override def getAggregate(acc: Acc[V]): V = {
    if (acc.res == null)
      acc.res = acc.vs.foldLeft(init)(join)
    acc.res
  }

  override def aggregateStream(str: stream.Stream[V]): V =
    str.iterator().asScala.foldLeft(init)(join)
}
