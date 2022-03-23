package inca.debugger.table

import inca.util.datastructure.BTree
import scala.collection.mutable
import scala.reflect.ClassTag

class BTreeTable[V: ClassTag](
    cols: Seq[String],
    minDegree: Int = 256,
    indexOrder: Seq[Int] = Seq()
  )(implicit val valOrd: Ordering[V])
    extends MutableTable[V] {

  val _indexOrder: Seq[Int] = if (indexOrder.isEmpty) cols.indices else indexOrder

  implicit private val tupleOrd: Ordering[Tuple] = new Ordering[Tuple] {
    override def compare(x: Tuple, y: Tuple): Int = {
      _indexOrder.foreach { idx =>
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

  private var tree: BTree[Tuple] = BTree.empty[Tuple](minDegree)
  private val _columns: mutable.ListBuffer[String] = mutable.ListBuffer.from(cols)

  override def columns: Seq[String] = _columns.toSeq
  override def entries: Seq[Tuple] = tree.entries
  override def contains(t: NamedTuple): Boolean = false
  override def contains(t: Tuple): Boolean = tree.contains(t)
  override def join(other: MutableTable[V]): Unit = {
    // we need to create a new table
  }
  override def insert(t: Tuple): Unit = tree.insert(t)
  override def toString: String = {
    s"${cols.mkString(", ")}, $tree"
  }
}
