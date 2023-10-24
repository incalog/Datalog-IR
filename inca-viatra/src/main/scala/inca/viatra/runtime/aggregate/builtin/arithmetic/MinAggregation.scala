package inca.viatra.runtime.aggregate.builtin.arithmetic

import inca.viatra.runtime.aggregate.Aggregation

object MinIntAggregation extends Aggregation[Int]:
  override val name: String = "min"
  override def init: Int = Int.MaxValue
  override def join(v1: Int, v2: Int): Int = v1.min(v2)
  override val isAssociative: Boolean = true
  override val isCommutative: Boolean = true
  override val hasUnjoin: Boolean = false

object MinDoubleAggregation extends Aggregation[Double]:
  override val name: String = "min"
  override def init: Double = Double.MaxValue
  override def join(v1: Double, v2: Double): Double = v1.min(v2)
  override val isAssociative: Boolean = true
  override val isCommutative: Boolean = true
  override val hasUnjoin: Boolean = false

