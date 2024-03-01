package inca.viatra.runtime.aggregate

import org.eclipse.viatra.query.runtime.matchers.psystem.aggregations.IMultisetAggregationOperator

import java.util.stream
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

case class AggState[ST,A](var st: ST, as: mutable.Set[A])

/** An aggregator for operations that are associative and commutative */
class AggregatorMono[ST, In, Out](val mono: MonoAggregation[ST, In, Out]) extends IMultisetAggregationOperator[In, AggState[ST,In], Out] {

  override def getShortDescription: String = mono.name
  override def getName: String = mono.name

  override def createNeutral(): AggState[ST,In] = AggState(mono.init, mutable.Set())
  override def isNeutral(acc: AggState[ST,In]): Boolean = acc.as.isEmpty

  override def contains(value: In, accumulator: AggState[ST, In]): Boolean = accumulator.as.contains(value)

  override def clone(original: AggState[ST, In]): AggState[ST, In] =
    AggState(original.st, original.as.clone())

  override def update(acc: AggState[ST,In], a: In, isInsertion: Boolean): AggState[ST,In] = {
    if (isInsertion) {
      acc.st = mono.add(acc.st, a)
      acc.as += a
    } else {
      acc.as -= a
      acc.st = acc.as.foldRight(mono.init)((a, st) => mono.add(st,a))
    }
    acc
  }

  override def combine(left: Out, right: AggState[ST, In]): Out =
    mono.combine(left, mono.result(right.st))

  override def getAggregate(acc: AggState[ST, In]): Out =
    mono.result(acc.st)

  override def aggregateStream(str: stream.Stream[In]): Out =
    val st = str.iterator().asScala.foldLeft(mono.init)(mono.add)
    mono.result(st)
}