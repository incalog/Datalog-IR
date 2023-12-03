package inca.viatra.runtime.aggregate.builtin.arithmetic

import inca.viatra.runtime.aggregate.MonoAggregation

class SumIntMono extends MonoAggregation[Int, Int] {
  override val name: String = "SumMono"
  override def init: Int = 0
  override def add(st: Int, a: Int): Int =
    require(a >= 0)
    st + a
}
