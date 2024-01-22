package inca.viatra.runtime.aggregate.builtin.arithmetic

import inca.viatra.runtime.aggregate.MonoAggregation

class SumDoubleMono extends MonoAggregation[Double, Double]{
  override val name: String = "SumDoubleMono"
  override def init: Double = 0.0
  override def add(st: Double, a: Double): Double = st + a
}
