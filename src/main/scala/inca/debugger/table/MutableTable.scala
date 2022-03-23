package inca.debugger.table

import inca.debugger.table.MutableTable.IndexOrder

trait MutableTable[V] {
  type Tuple = Seq[V]
  type NamedTuple = Map[String, V]

  def columns: Seq[String]
  def entries: Seq[Tuple]
  def contains(t: NamedTuple): Boolean
  def contains(t: Tuple, indexOrder: Option[IndexOrder] = None): Boolean
  def join(other: MutableTable[V]): Unit
  def insert(t: Tuple): Unit
}

object MutableTable {
  type IndexOrder = Seq[Int]

  def constructTupleOrdering[V](order: IndexOrder)(implicit valOrd: Ordering[V]): Ordering[Seq[V]] =
    new Ordering[Seq[V]] {
      override def compare(x: Seq[V], y: Seq[V]): Int = {
        order.foreach { idx =>
          val xVal = x(idx)
          val yVal = y(idx)
          val vCompare = valOrd.compare(xVal, yVal)
          if (vCompare < 0) {
            return -1
          } else if (vCompare > 0) {
            return 1
          }
        }
        0
      }
    }
}
