package inca.viatra.runtime.aggregate.builtin.arithmetic

import inca.viatra.runtime.aggregate.MonoAggregation

class MaxDoubleMono extends MonoAggregation[Double, Double, Double] {
  override val name: String = "MaxDoubleMono"

  override def init: Double = 0.0

  override def add(st: Double, a: Double): Double = if a > st then a else st

  override def result(st: Double): Double = st

  override def combine(o1: Double, o2: Double): Double = o1 max o2
}
