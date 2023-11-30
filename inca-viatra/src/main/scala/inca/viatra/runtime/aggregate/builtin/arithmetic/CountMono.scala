package inca.viatra.runtime.aggregate.builtin.arithmetic

import inca.viatra.runtime.aggregate.MonoAggregation

class CountMono extends MonoAggregation[Int, Any, Int]{
  override val name: String = "CountMono"

  override def init: Int = 0

  override def add(st: Int, a: Any): Int =
    st + 1

  override def result(st: Int): Int = st
}
