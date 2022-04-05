package inca.debugger.table

import inca.debugger.table.indexing.IndexCover
import inca.util.datastructure.BTree
import scala.reflect.ClassTag

trait ImmutableTable[V] extends NewTable[V] {

  def insert(t: Tuple): ImmutableTable[V]

  def union(other: Seq[Tuple]): ImmutableTable[V]
  def union(other: ImmutableTable[V]): ImmutableTable[V]
  def diff(other: ImmutableTable[V]): ImmutableTable[V]
  def cartesian(other: ImmutableTable[V]): ImmutableTable[V]
  def project(cols: Seq[String]): ImmutableTable[V]
  def select(f: Tuple => Boolean): ImmutableTable[V]
  def join(other: ImmutableTable[V]): ImmutableTable[V]

  def rename(subst: Map[String, String]): ImmutableTable[V]
  def projectAndRename(subst: Map[String, String]): ImmutableTable[V]

  def bindingsToString(f: V => String): String = {
    val rowStrings = entries.map { row =>
      val sb = new StringBuilder
      sb += '['
      columns.foreach { col =>
        val ix = columnIndex(col)
        val v = row(ix)
        if (v != null) {
          sb ++= col
          sb += '='
          sb ++= f(v)
          sb ++= ", "
        }
      }
      if (sb.length() > 2) {
        sb.deleteCharAt(sb.length() - 1)
        sb.deleteCharAt(sb.length() - 1)
      }
      sb += ']'
      sb.toString()
    }
    rowStrings.size match {
      case 0 => "nil"
      case 1 => rowStrings.head
      case _ => rowStrings.mkString("{", ", ", "}")
    }
  }
}
object ImmutableTable {
  def empty[V: ClassTag](
      columns: Seq[String],
      indexCovers: Set[IndexCover] = Set(),
      minDegree: Int = 256
    )(implicit valOrdering: Ordering[V],
      topAndBotFactory: () => (V, V)
    ): ImmutableBTreeTable[V] = {
    ImmutableBTreeTable.apply(columns, Seq(), indexCovers, minDegree)
  }

  def unit[V: ClassTag](
      indexCovers: Set[IndexCover] = Set(),
      minDegree: Int = 256
    )(implicit valOrdering: Ordering[V],
      topAndBotFactory: () => (V, V)
    ): ImmutableBTreeTable[V] = {
    ImmutableBTreeTable.apply(Seq(), Seq(Seq()), indexCovers, minDegree)
  }

  def apply[V: ClassTag](
      columns: Seq[String],
      entries: Seq[Seq[V]],
      indexCovers: Set[IndexCover] = Set(),
      minDegree: Int = 256
    )(implicit valOrdering: Ordering[V],
      topAndBotFactory: () => (V, V)
    ): ImmutableBTreeTable[V] = {
    ImmutableBTreeTable(columns, entries, indexCovers, minDegree)
  }
}

class ImmutableBTreeTable[V: ClassTag](
    cols: Seq[String],
    indexCovers: Set[IndexCover],
    minDegree: Int = 256
  )(implicit val valOrdering: Ordering[V],
    implicit val topAndBotFactory: () => (V, V))
    extends ImmutableTable[V] {

  var indices: Map[IndexCover, BTree[Tuple]] = indexCovers.map { cover =>
    val indexOrder = cover.toIndexOrder(cols)
    implicit val tupleOrdering: Ordering[Tuple] = indexOrder.tupleOrdering
    cover -> BTree.empty[Tuple](minDegree)
  }.toMap

  override def columns: Seq[String] = cols
  override def isBound(column: String): Boolean = cols.contains(column)

  override def isEmpty: Boolean = indices(indexCovers.head).size == 0
  override def size: Int = indices(indexCovers.head).size

  override def columnIndex(column: String): Int = columns.indexOf(column)

  override def entries(indexCover: IndexCover): Seq[Tuple] = indices(indexCover).entries
  override def entries: Seq[Tuple] = entries(indices.keys.head)

  override def entries(namedTuple: NamedTuple): Seq[Tuple] = {
    // select appropriate index
    val search = columns.filter(namedTuple.contains)
    val indexCover = selectIndexCover(search)

    if (namedTuple.size == columns.size) {
      val tuple = columns.map(namedTuple)
      if (indices(indexCover).contains(tuple)) Seq(tuple)
      else Nil
    } else {
      // prepare lower and upper bound
      val (top, bot) = topAndBotFactory()
      val lower = columns.map { col =>
        namedTuple.getOrElse(col, bot)
      }
      val upper = columns.map { col =>
        namedTuple.getOrElse(col, top)
      }
      // do lexical search on appropriate index
      // we want to return the unit table if we query a unit table
      // Normally we would need query BotList and TopList to get the correct result
      // TODO fix
      indices(indexCover).lexSearch(lower, upper)
    }
  }

  private def selectIndexCover(search: Seq[String]): IndexCover = {
    indexCovers.find { cover =>
      cover.order.startsWith(search)
    }.getOrElse(
      throw new IllegalArgumentException(s"We could not find an index that covers search $search"))
  }

  override def insert(t: Tuple): ImmutableTable[V] = {
    val newTable = new ImmutableBTreeTable[V](columns, indexCovers, minDegree)

    val newIndices = indices.map { case (indexCover, btree) =>
      val copy = btree.deepCopy()
      copy.insert(t)
      indexCover -> copy
    }
    newTable.indices = newIndices
    newTable
  }
  override def contains(t: Tuple, indexOrder: IndexCover): Boolean = indices(indexOrder).contains(t)
  override def contains(t: NamedTuple): Boolean = { true }

  override def union(other: ImmutableTable[V]): ImmutableBTreeTable[V] = {
    val newEntries = entries ++ other.entries
    ImmutableBTreeTable[V](columns, newEntries, indexCovers, minDegree)
  }

  override def union(other: Seq[Tuple]): ImmutableBTreeTable[V] = {
    val newEntries = entries ++ other
    ImmutableBTreeTable[V](columns, newEntries, indexCovers, minDegree)
  }

  override def diff(other: ImmutableTable[V]): ImmutableBTreeTable[V] = {
    val newEntries = entries.diff(other.entries)
    ImmutableBTreeTable[V](columns, newEntries, indexCovers, minDegree)
  }

  override def cartesian(other: ImmutableTable[V]): ImmutableBTreeTable[V] = {
    null
  }

  override def project(newCols: Seq[String]): ImmutableBTreeTable[V] = {
    if (newCols == columns) {
      // when we want to project the whole table we return the table instead
      this
    } else {
      val colsIndex = newCols.map(columnIndex)
      val projectedIndexCovers = indexCovers.map { cover =>
        IndexCover(cover.order.filter(newCols.contains))
      }
      val newEntries = entries.map(colsIndex.map)
      ImmutableBTreeTable[V](newCols, newEntries, projectedIndexCovers, minDegree)
    }
  }

  override def select(f: Tuple => Boolean): ImmutableBTreeTable[V] = {
    val newEntries = entries.filter(f)
    ImmutableBTreeTable[V](cols, newEntries, indexCovers, minDegree)
  }

  override def join(other: ImmutableTable[V]): ImmutableBTreeTable[V] = {
    val otherNewCols = other.columns.filter { col =>
      !columns.contains(col)
    }
    val otherNewColsIndices = otherNewCols.map(other.columns.indexOf)
    val sameCols = other.columns.filter(columns.contains)
    val newEntries =
      for {
        entry <- entries
        // TODO we need to consider all bound values of other not of this
        namedEntry = columns.zip(entry).toMap
        otherNamedEntry = namedEntry.filter { case (k, _) => sameCols.contains(k) }
        // use lexical search to efficiently query inner table
        // this is only efficient as long as there is an appropriate index
        otherEntry <- other.entries(otherNamedEntry)
      } yield {
        entry ++ otherNewColsIndices.map(otherEntry.apply)
      }
    ImmutableBTreeTable[V](columns ++ otherNewCols, newEntries, indexCovers, minDegree)
  }

  override def rename(subst: Map[String, String]): ImmutableBTreeTable[V] = {
    val newColumns = columns.map(subst)
    val newIndexCovers = indexCovers.map { cover => IndexCover(cover.order.map(subst)) }
    // entries should stay the same as this was only a renaming of columns
    ImmutableBTreeTable[V](newColumns, entries, newIndexCovers, minDegree)
  }

  override def projectAndRename(subst: Map[String, String]): ImmutableBTreeTable[V] = {
    // project and rename columns
    val newColumns = columns.flatMap(subst.get)

    // project and rename indices
    val newIndexCovers = indexCovers.flatMap { cover =>
      val newOrder = cover.order.flatMap(subst.get)
      if (newOrder.nonEmpty) Some(IndexCover(newOrder))
      else None
    }

    // project rows
    val projectedCols = columns.filter(subst.contains)
    val colsIndex = projectedCols.map(columnIndex)
    val newEntries = entries.map(colsIndex.map)

    // construct new table
    ImmutableBTreeTable[V](newColumns, newEntries, newIndexCovers, minDegree)
  }

  override def toString: String = {
    s"""ImmutableTable(
      |  ${columns.mkString(", ")}
      |  ${entries.map(_.mkString(", ")).mkString("\n  ")}
      |)
      |""".stripMargin
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: ImmutableBTreeTable[V] =>
      // we can use a simple equality test because entries returns a sorted seq
      columns == other.columns && size == other.size && entries == other.entries
    case _ => false
  }

}

object ImmutableBTreeTable {
  def apply[V: ClassTag](
      cols: Seq[String],
      entries: Seq[Seq[V]],
      indexCovers: Set[IndexCover] = Set(),
      minDegree: Int = 256
    )(implicit valOrdering: Ordering[V],
      topAndBotFactory: () => (V, V)
    ): ImmutableBTreeTable[V] = {
    val _indexCovers =
      if (indexCovers.isEmpty) Set(IndexCover(cols))
      else indexCovers
    val table = new ImmutableBTreeTable[V](cols, _indexCovers, minDegree)

    table.indices.foreach { case (indexCover, tree) =>
      val indexOrder = indexCover.toIndexOrder(cols)
      // sorting the entries should make for most efficient insertion
      implicit val tupleOrd: Ordering[Seq[V]] = indexOrder.tupleOrdering
      val sortedEntries = entries.sorted
      sortedEntries.foreach(tree.insert)
    }
    table
  }
}
