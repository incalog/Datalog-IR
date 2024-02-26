package inca.viatra.runtime.aggregate.builtin.arithmetic

import inca.viatra.runtime.aggregate.MonoAggregation

class MaxIntMono extends MonoAggregation[Int, Int, Int] {
  override val name: String = "MaxMono"
  override def init: Int = 0
  override def add(st: Int, a: Int): Int = if st < a then a else st
  override def result(st: Int): Int = st
  override def combine(o1: Int, o2: Int): Int = o1 max o2
}

