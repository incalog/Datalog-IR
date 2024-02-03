package inca.viatra.runtime.aggregate

import org.eclipse.viatra.query.runtime.matchers.psystem.aggregations.IMultisetAggregationOperator

import java.util.stream
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

case class AggState[ST,A](var st: ST, as: mutable.Set[A])

/** An aggregator for operations that are associative and commutative */
class AggregatorMono[ST, A](val agg: MonoAggregation[ST, A]) extends IMultisetAggregationOperator[A, AggState[ST,A], ST] {

  override def getShortDescription: String = agg.name
  override def getName: String = agg.name

  override def createNeutral(): AggState[ST,A] = AggState(agg.init, mutable.Set())
  override def isNeutral(acc: AggState[ST,A]): Boolean = acc.as.isEmpty

  override def contains(value: A, accumulator: AggState[ST, A]): Boolean = accumulator.as.contains(value)

  override def clone(original: AggState[ST, A]): AggState[ST, A] =
    AggState(original.st, original.as.clone())

  override def update(acc: AggState[ST,A], a: A, isInsertion: Boolean): AggState[ST,A] = {
    if (isInsertion) {
      acc.st = agg.add(acc.st, a)
      acc.as += a
    } else {
      acc.as -= a
      acc.st = acc.as.foldRight(agg.init)((a,st) => agg.add(st,a))
    }
    acc
  }

  override def combine(left: ST, right: AggState[ST, A]): ST =
    right.as.foldLeft(left)(agg.add)

  override def getAggregate(acc: AggState[ST, A]): ST =
    acc.st

  override def aggregateStream(str: stream.Stream[A]): ST =
    str.iterator().asScala.foldLeft(agg.init)(agg.add)
}