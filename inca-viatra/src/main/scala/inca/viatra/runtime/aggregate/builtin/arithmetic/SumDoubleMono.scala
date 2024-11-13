package inca.viatra.runtime.aggregate.builtin.arithmetic

import inca.viatra.runtime.aggregate.MonoAggregation

class SumDoubleMono extends MonoAggregation[Double, Double, Double] {
  override val name: String = "SumDoubleMono"

  override def init: Double = 0.0

  override def add(st: Double, a: Double): Double = st + a

  override def result(st: Double): Double = st

  override def combine(o1: Double, o2: Double): Double = o1 + o2
}
