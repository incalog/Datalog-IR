package inca.debugger.table.indexing

case class IndexOrder(indices: Seq[Int]) {
  def tupleOrdering[V](implicit valOrd: Ordering[V]): Ordering[Seq[V]] =
    new Ordering[Seq[V]] {
      override def compare(x: Seq[V], y: Seq[V]): Int = {
        indices.foreach { idx =>
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
