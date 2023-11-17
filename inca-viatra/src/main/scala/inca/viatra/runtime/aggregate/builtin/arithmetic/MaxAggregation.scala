package inca.viatra.runtime.aggregate.builtin.arithmetic

import inca.viatra.runtime.aggregate.JoinAggregation

object MaxIntAggregation extends JoinAggregation[Int]:
  override val name: String = "max"
  override def init: Int = Int.MinValue
  override def join(v1: Int, v2: Int): Int = v1.max(v2)
  override val isAssociative: Boolean = true
  override val isCommutative: Boolean = true
  override val hasUnjoin: Boolean = false

object MaxDoubleAggregation extends JoinAggregation[Double]:
  override val name: String = "max"
  override def init: Double = Double.MinValue
  override def join(v1: Double, v2: Double): Double = v1.max(v2)
  override val isAssociative: Boolean = true
  override val isCommutative: Boolean = true
  override val hasUnjoin: Boolean = false

