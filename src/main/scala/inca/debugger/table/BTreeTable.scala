package inca.debugger.table

import inca.debugger.table.MutableTable.constructTupleOrdering
import inca.debugger.table.MutableTable.IndexOrder
import inca.util.datastructure.BTree
import scala.collection.mutable
import scala.reflect.ClassTag

class BTreeTable[V: ClassTag](
    cols: Seq[String],
    minDegree: Int = 256,
    _indexOrders: Seq[IndexOrder] = Seq()
  )(implicit val valOrd: Ordering[V])
    extends MutableTable[V] {

  private val _columns: mutable.ListBuffer[String] = mutable.ListBuffer.from(cols)
  val indexOrders: Seq[IndexOrder] =
    if (_indexOrders.isEmpty) Seq(columns.indices) else _indexOrders
  private val defaultIndexOrder: IndexOrder = indexOrders.head

  private val indices: Map[IndexOrder, BTree[Tuple]] = indexOrders.map { io =>
    implicit val tupleOrder: Ordering[Tuple] = constructTupleOrdering(io)
    io -> BTree.empty[Tuple](minDegree)
  }.toMap

  override def columns: Seq[String] = _columns.toSeq
  override def entries: Seq[Tuple] = indices(defaultIndexOrder).entries
  override def contains(t: NamedTuple): Boolean = false
  override def contains(t: Tuple, indexOrder: Option[IndexOrder] = None): Boolean =
    indices(indexOrder.getOrElse(defaultIndexOrder)).contains(t)

  override def join(other: MutableTable[V]): Unit = {
    // we need to create a new table
  }

  override def insert(t: Tuple): Unit = indices.foreach { case (_, tree) => tree.insert(t) }

  override def toString: String = {
    s"${cols.mkString(", ")}, ${indices(defaultIndexOrder)}"
  }
}
