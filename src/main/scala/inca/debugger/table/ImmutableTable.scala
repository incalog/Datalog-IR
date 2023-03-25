package inca.debugger.table

import inca.debugger.table.indexing.IndexCover
import inca.util.datastructure.BTree
import scala.reflect.ClassTag

trait ImmutableTable[V] extends Table[V] {

  def insert(t: Tuple, resultIndices: Set[IndexCover] = Set()): ImmutableTable[V]

  def indexCovers: Set[IndexCover]

  // def union(other: Seq[Tuple], resultIndices: Set[IndexCover] = Set()): ImmutableTable[V]
  def union(other: ImmutableTable[V], resultIndices: Set[IndexCover] = Set()): ImmutableTable[V]
  def diff(other: ImmutableTable[V], resultIndices: Set[IndexCover] = Set()): ImmutableTable[V]
  def cartesian(other: ImmutableTable[V], resultIndices: Set[IndexCover] = Set()): ImmutableTable[V]
  def project(cols: Seq[String], resultIndices: Set[IndexCover] = Set()): ImmutableTable[V]
  def select(f: Tuple => Boolean, resultIndices: Set[IndexCover] = Set()): ImmutableTable[V]
  def join(other: ImmutableTable[V], resultIndices: Set[IndexCover] = Set()): ImmutableTable[V]

  def antiJoin(other: ImmutableTable[V], resultIndices: Set[IndexCover] = Set()): ImmutableTable[V]
  def rename(subst: Map[String, String], resultIndices: Set[IndexCover] = Set()): ImmutableTable[V]
  def projectAndRename(
      subst: Map[String, String],
      resultIndices: Set[IndexCover] = Set()
    ): ImmutableTable[V]

  def bindingsToString(f: V => String, numOfRowsShown: Int): String = {
    val rowStrings = entries.take(numOfRowsShown).map { row =>
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
      minDegree: Int = BTree.GlobalMinDegree
    )(implicit valOrdering: Ordering[V],
      topAndBotFactory: () => (V, V)
    ): ImmutableBTreeTable[V] = {
    ImmutableBTreeTable.apply(columns, Seq(), indexCovers, minDegree)
  }

  def unit[V: ClassTag](
      indexCovers: Set[IndexCover] = Set(),
      minDegree: Int = BTree.GlobalMinDegree
    )(implicit valOrdering: Ordering[V],
      topAndBotFactory: () => (V, V)
    ): ImmutableBTreeTable[V] = {
    ImmutableBTreeTable.apply(Seq(), Seq(Seq()), indexCovers, minDegree)
  }

  def apply[V: ClassTag](
      columns: Seq[String],
      entries: Seq[Seq[V]],
      indexCovers: Set[IndexCover] = Set(),
      minDegree: Int = BTree.GlobalMinDegree
    )(implicit valOrdering: Ordering[V],
      topAndBotFactory: () => (V, V)
    ): ImmutableBTreeTable[V] = {
    ImmutableBTreeTable(columns, entries, indexCovers, minDegree)
  }
}

class ImmutableBTreeTable[V: ClassTag](
    cols: Seq[String],
    override val indexCovers: Set[IndexCover],
    minDegree: Int = BTree.GlobalMinDegree
  )(implicit val valOrdering: Ordering[V],
    implicit val topAndBotFactory: () => (V, V))
    extends ImmutableTable[V] {

  var indices: Map[IndexCover, BTree[Tuple]] = indexCovers.map { cover =>
    val indexOrder = cover.toIndexOrder(cols)
    implicit val tupleOrdering: Ordering[Tuple] = indexOrder.tupleOrdering
    cover -> BTree.empty[Tuple](minDegree)
  }.toMap

  private[ImmutableBTreeTable] def bulkLoad(entries: Seq[Tuple]): Unit = {
    indices = indexCovers.map { cover =>
      val indexOrder = cover.toIndexOrder(cols)
      implicit val tupleOrdering: Ordering[Tuple] = indexOrder.tupleOrdering
      cover -> BTree(entries.distinct.sorted)
    }.toMap
  }

  override lazy val columns: Seq[String] = cols
  override def isBound(column: String): Boolean = cols.contains(column)

  override lazy val isEmpty: Boolean = indices(indexCovers.head).size == 0
  override lazy val isUnit: Boolean = columns.isEmpty && entries.size == 1 && entries.head == Seq()
  override lazy val size: Int = indices(indexCovers.head).size

  override def columnIndex(column: String): Int = columns.indexOf(column)

  override def entries(indexCover: IndexCover): Seq[Tuple] = indices(indexCover).entries
  override def entries: Seq[Tuple] = entries(indices.keys.head)

  override def entries(namedTuple: NamedTuple): Seq[Tuple] = {
    // select appropriate index
    val search = columns.filter(namedTuple.contains)
    val btree = selectIndexCover(search) match {
      case Some(indexCover) => indices(indexCover)
      case None =>
        // this is inefficient, we construct new tree because the the appropriate index was not available
        // TODO hOW can we know at each construction of table what index is needed in the future?
        val indexCover = IndexCover(namedTuple.map(_._1))
        implicit val tupleOrdering: Ordering[Tuple] = indexCover.toIndexOrder(columns).tupleOrdering
        BTree[Tuple](entries.sorted)
    }
    if (namedTuple.size == columns.size) {
      val tuple = columns.map(namedTuple.toMap)
      if (btree.contains(tuple)) Seq(tuple)
      else Nil
    } else {
      // prepare lower and upper bound
      val (top, bot) = topAndBotFactory()
      val lower = columns.map { col =>
        namedTuple.toMap.getOrElse(col, bot)
      }
      val upper = columns.map { col =>
        namedTuple.toMap.getOrElse(col, top)
      }
      // do lexical search on appropriate index
      btree.lexSearch(lower, upper)
    }
  }

  private def selectIndexCover(search: Seq[String]): Option[IndexCover] =
    indexCovers.find { cover =>
      cover.order.startsWith(search)
    }

  private def selectResultIndices(resultIndices: Set[IndexCover]): Set[IndexCover] =
    if (resultIndices.isEmpty) indexCovers
    else resultIndices

  override def insert(t: Tuple, resultIndices: Set[IndexCover] = Set()): ImmutableTable[V] = {
    if (columns.size != t.size)
      throw new IllegalArgumentException("Not possible to insert")
    val newTable = new ImmutableBTreeTable[V](columns, indexCovers, minDegree)

    val newIndexCovers = selectResultIndices(resultIndices)

    // use old indices if possible, create new btree if necessary
    val newIndices = newIndexCovers.map { indexCover =>
      indices.get(indexCover) match {
        case Some(btree) =>
          val copy = btree.deepCopy()
          copy.insert(t)
          indexCover -> copy
        case None =>
          val newEntries = t +: entries
          implicit val tupleOrdering: Ordering[Seq[V]] =
            indexCover.toIndexOrder(columns).tupleOrdering
          val btree = BTree(newEntries, minDegree)
          indexCover -> btree
      }
    }.toMap

    newTable.indices = newIndices
    newTable
  }
  override def contains(t: Tuple, indexOrder: IndexCover): Boolean = indices(indexOrder).contains(t)
  override def contains(t: NamedTuple): Boolean = {
    val tuple = columns.flatMap(t.toMap.get)
    indices(indexCovers.head).contains(tuple)
  }

  override def union(
      other: ImmutableTable[V],
      resultIndices: Set[IndexCover] = Set()
    ): ImmutableBTreeTable[V] = {
    if (columns.size != other.columns.size)
      throw new IllegalArgumentException("Not possible to union")
    val newEntries =
      if (columns == other.columns) {
        entries ++ other.entries
      } else {
        val indexMap = columns.map(other.columns.indexOf)
        def rearrange(entry: Seq[V]): Seq[V] = {
          indexMap.map(entry)
        }
        entries ++ other.entries.map(rearrange)
      }
    val newIndexCovers = selectResultIndices(resultIndices)
    ImmutableBTreeTable[V](columns, newEntries, newIndexCovers, minDegree)
  }

//  override def union(
//      other: Seq[Tuple],
//      resultIndices: Set[IndexCover] = Set()
//    ): ImmutableBTreeTable[V] = {
//    val newEntries = entries ++ other
//    ImmutableBTreeTable[V](columns, newEntries, indexCovers, minDegree)
//  }

  override def diff(
      other: ImmutableTable[V],
      resultIndices: Set[IndexCover] = Set()
    ): ImmutableBTreeTable[V] = {
    if (columns.size != other.columns.size)
      throw new IllegalArgumentException("Not possible to diff")
    val newIndexCovers = selectResultIndices(resultIndices)
    val newEntries =
      if (columns == other.columns) {
        entries.diff(other.entries)
      } else {
        val indexMap = columns.map(other.columns.indexOf)
        def rearrange(entry: Seq[V]): Seq[V] = {
          indexMap.map(entry)
        }
        entries.diff(other.entries.map(rearrange))
      }
    ImmutableBTreeTable[V](columns, newEntries, newIndexCovers, minDegree)
  }

  override def cartesian(
      other: ImmutableTable[V],
      resultIndices: Set[IndexCover] = Set()
    ): ImmutableBTreeTable[V] = {
    null
  }

  override def project(
      newCols: Seq[String],
      resultIndices: Set[IndexCover] = Set()
    ): ImmutableBTreeTable[V] = {

    val newIndexCovers =
      if (resultIndices.isEmpty)
        indexCovers.map { cover =>
          IndexCover(cover.order.filter(newCols.contains))
        }
      else
        resultIndices
    if (newCols == columns && newIndexCovers == indexCovers) {
      // when we want to project the whole table we return the table instead
      this
    } else {
      val colsIndex = newCols.map(columnIndex)
      val newEntries = entries.map(colsIndex.map)
      ImmutableBTreeTable[V](newCols, newEntries, newIndexCovers, minDegree)
    }
  }

  override def select(
      f: Tuple => Boolean,
      resultIndices: Set[IndexCover] = Set()
    ): ImmutableBTreeTable[V] = {
    val newIndexCovers = selectResultIndices(resultIndices)
    val newEntries = entries.filter(f)
    ImmutableBTreeTable[V](cols, newEntries, newIndexCovers, minDegree)
  }

  override def join(
      other: ImmutableTable[V],
      resultIndices: Set[IndexCover] = Set()
    ): ImmutableBTreeTable[V] = {
    val otherNewCols = other.columns.filter { col =>
      !columns.contains(col)
    }
    val otherNewColsIndices = otherNewCols.map(other.columns.indexOf)
    val sameCols = other.columns.filter(columns.contains)
    val newEntries =
      if (sameCols.isEmpty) {
        // this is a cartesian product
        // we cannot apply an efficient join technique
        for {
          entry <- entries
          otherEntry <- other.entries
        } yield {
          entry ++ otherEntry
        }
      } else {
        for {
          entry <- entries
          namedEntry = columns.zip(entry)
          otherNamedEntry = namedEntry.filter { case (k, _) => sameCols.contains(k) }
          // use lexical search to efficiently query inner table
          // this is only efficient as long as there is an appropriate index
          otherEntry <- other.entries(otherNamedEntry)
        } yield {
          entry ++ otherNewColsIndices.map(otherEntry.apply)
        }
      }
    val newIndexCovers = selectResultIndices(resultIndices)
    ImmutableBTreeTable[V](columns ++ otherNewCols, newEntries, newIndexCovers, minDegree)
  }

  def antiJoin(
      other: ImmutableTable[V],
      resultIndices: Set[IndexCover] = Set()
    ): ImmutableTable[V] = {
    val sameCols = other.columns.filter(columns.contains)
    val newEntries =
      for {
        entry <- entries
        namedEntry = columns.zip(entry)
        otherNamedEntry = namedEntry.filter { case (k, _) => sameCols.contains(k) }
        // use lexical search to efficiently query inner table
        // this is only efficient as long as there is an appropriate index
        if other.entries(otherNamedEntry).isEmpty
      } yield {
        entry
      }
    val newIndexCovers = selectResultIndices(resultIndices)
    ImmutableBTreeTable[V](columns, newEntries, newIndexCovers, minDegree)
  }

  override def rename(
      subst: Map[String, String],
      resultIndices: Set[IndexCover] = Set()
    ): ImmutableBTreeTable[V] = {
    val newColumns = columns.map(subst)
    val newIndexCovers =
      if (resultIndices.isEmpty)
        indexCovers.map { cover => IndexCover(cover.order.map(subst)) }
      else
        resultIndices

    // entries should stay the same as this was only a renaming of columns
    ImmutableBTreeTable[V](newColumns, entries, newIndexCovers, minDegree)
  }

  override def projectAndRename(
      subst: Map[String, String],
      resultIndices: Set[IndexCover] = Set()
    ): ImmutableBTreeTable[V] = {
    // project and rename columns
    val newColumns = columns.flatMap(subst.get)

    // project and rename indices
    val newIndexCovers =
      if (resultIndices.isEmpty)
        indexCovers.flatMap { cover =>
          val newOrder = cover.order.flatMap(subst.get)
          if (newOrder.nonEmpty) Some(IndexCover(newOrder))
          else None
        }
      else
        resultIndices

    // project rows
    val projectedCols = columns.filter(subst.contains)
    val colsIndex = projectedCols.map(columnIndex)
    val newEntries = entries.map(colsIndex.map)

    // construct new table
    ImmutableBTreeTable[V](newColumns, newEntries, newIndexCovers, minDegree)
  }

  override lazy val toString: String = {
    if (size == 0) {
      s"EmptyTable(${columns.mkString(", ")})"
    } else if (columns.isEmpty && size == 1) {
      s"UnitTable"
    } else {
      s"""ImmutableTable(
        |  ${columns.mkString(", ")}
        |  ${entries.map(_.mkString("\t")).mkString("\n  ")}
        |)""".stripMargin
    }
  }

  private lazy val _hashCode = {
    val columnsHash = columns.##
    val entriesHash = entries.##
    31 * ((31 + columnsHash) + entriesHash)
  }

  override def hashCode(): Int = {
    _hashCode
  }

//  override def hashCode(): Int = {}

  override def equals(obj: Any): Boolean = obj match {
    case other: ImmutableBTreeTable[V] =>
      // we can use a simple equality test because entries returns a sorted seq
      this.columns.forall(other.columns.contains) && size == other.size && entries == other.entries
    case _ => false
  }

  override def subset(other: Table[V]): Boolean = {
    this.columns.forall(other.columns.contains) && this.size <= other.size && this.entries.forall {
      tuple =>
        val named = this.columns.zip(tuple)
        other.contains(named)
    }
  }
}

object ImmutableBTreeTable {
  def apply[V: ClassTag](
      cols: Seq[String],
      entries: Seq[Seq[V]],
      indexCovers: Set[IndexCover] = Set(),
      minDegree: Int = BTree.GlobalMinDegree
    )(implicit valOrdering: Ordering[V],
      topAndBotFactory: () => (V, V)
    ): ImmutableBTreeTable[V] = {
    val _indexCovers =
      if (indexCovers.isEmpty) Set(IndexCover(cols))
      else indexCovers
    val table = new ImmutableBTreeTable[V](cols, _indexCovers, minDegree)
    table.bulkLoad(entries)
    table
  }
}
