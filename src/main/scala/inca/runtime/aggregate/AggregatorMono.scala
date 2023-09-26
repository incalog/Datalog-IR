package inca.runtime.aggregate

import org.eclipse.viatra.query.runtime.matchers.psystem.aggregations.IMultisetAggregationOperator

import java.util.stream
import scala.jdk.CollectionConverters._

case class AggState[ST,A](st: ST, as: List[A])

/** An aggregator for operations that are associative and commutative */
class AggregatorMono[ST, A, B](val agg: MonoAggregation[ST, A, B]) extends IMultisetAggregationOperator[A, AggState[ST,A], ST] {

  override def getShortDescription: String = agg.name
  override def getName: String = agg.name

  override def createNeutral(): AggState[ST,A] = AggState(agg.init, List())
  override def isNeutral(acc: AggState[ST,A]): Boolean = acc.as.isEmpty

  override def update(acc: AggState[ST,A], a: A, isInsertion: Boolean): AggState[ST,A] = {
    if (isInsertion) {
      val newSt = agg.add(acc.st, a)
      AggState(newSt, a::acc.as)
    } else {
      val (prefix,suffix) = acc.as.span(_ != a)
      val newAs = prefix ++ suffix.tail
      val newSt = newAs.foldRight(agg.init)((a,st) => agg.add(st,a))
      AggState(newSt, newAs)
    }
  }

  override def getAggregate(acc: AggState[ST, A]): ST =
    acc.st

  override def aggregateStream(str: stream.Stream[A]): ST =
    str.iterator().asScala.foldLeft(agg.init)(agg.add)
}
