package inca.viatra.runtime.aggregate.builtin.arithmetic

import inca.viatra.runtime.aggregate.MonoAggregation

class CountFromMono(from: Int) extends MonoAggregation[Int, Any] {
  override val name: String = "CountFromMono"
  override def init: Int = 0
  override def add(st: Int, a: Any): Int =
    st + 1
}