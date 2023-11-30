package inca.viatra.runtime.aggregate.builtin.arithmetic

import inca.viatra.runtime.aggregate.JoinAggregation

class SumIntAggregation extends JoinAggregation[Int]:
  override val name: String = "sum"
  override def init: Int = 0
  override def join(v1: Int, v2: Int): Int = v1 + v2
  override val isAssociative: Boolean = true
  override val isCommutative: Boolean = true
  override val hasUnjoin: Boolean = false

class SumDoubleAggregation extends JoinAggregation[Double]:
  override val name: String = "sum"
  override def init: Double = 0.0
  override def join(v1: Double, v2: Double): Double = v1 + v2
  override val isAssociative: Boolean = true
  override val isCommutative: Boolean = true
  override val hasUnjoin: Boolean = false

