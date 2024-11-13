package inca.viatra.runtime.aggregate.builtin.arithmetic

import inca.viatra.runtime.aggregate.MonoAggregation

class CountFromMono(from: Int) extends MonoAggregation[Int, Any, Int] {
  override val name: String = "CountFromMono"

  override def init: Int = 0

  override def add(st: Int, a: Any): Int = st + 1

  override def result(st: Int): Int = st

  override def combine(o1: Int, o2: Int): Int = o1 + o2
}