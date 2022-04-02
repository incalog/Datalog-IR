package inca.debugger.table

import inca.debugger.table.indexing.IndexCover
import inca.util.datastructure.BTree
import scala.reflect.ClassTag

trait ImmutableTable[V] extends NewTable[V] {

  def insert(t: Tuple): ImmutableTable[V]

  def union(other: ImmutableTable[V]): ImmutableTable[V]
  def diff(other: ImmutableTable[V]): ImmutableTable[V]
  def cartesian(other: ImmutableTable[V]): ImmutableTable[V]
  def project(cols: Seq[String]): ImmutableTable[V]
  def select(f: Tuple => Boolean): ImmutableTable[V]
  def rename(subst: Map[String, String]): ImmutableTable[V]
  def join(other: ImmutableTable[V]): ImmutableTable[V]
}

class ImmutableBTreeTable[V: ClassTag](
    cols: Seq[String],
    minDegree: Int = 256,
    indexCovers: Set[IndexCover]
  )(implicit val valOrdering: Ordering[V],
    implicit val topAndBotFactory: () => (V, V))
    extends ImmutableTable[V] {

  var indices: Map[IndexCover, BTree[Tuple]] = indexCovers.map { cover =>
    val indexOrder = cover.toIndexOrder(cols)
    implicit val tupleOrdering: Ordering[Tuple] = indexOrder.tupleOrdering
    cover -> BTree.empty[Tuple](minDegree)
  }.toMap

  override def columns: Seq[String] = cols
  override def entries(indexCover: IndexCover): Seq[Tuple] = indices(indexCover).entries

  override def entries: Seq[Tuple] = entries(indices.keys.head)

  override def entries(namedTuple: NamedTuple): Seq[Tuple] = {
    // select appropriate index
    val indexCover = selectIndexCover(namedTuple.keys.toSeq)

    // prepare lower and upper bound
    val (top, bot) = topAndBotFactory()
    val lower = columns.map { col =>
      namedTuple.getOrElse(col, bot)
    }
    val upper = columns.map { col =>
      namedTuple.getOrElse(col, top)
    }

    // do lexical search on appropriate index
    indices(indexCover).lexSearch(lower, upper)
  }

  private def selectIndexCover(search: Seq[String]): IndexCover = {
    val usedCols = columns.filter(search.contains)
    val similarityScores = indexCovers.toSeq.map { cover =>
      val score = 0
      cover -> score
    }.toMap
    similarityScores.maxBy(_._2)._1
  }

  override def insert(t: Tuple): ImmutableTable[V] = {
    val newTable = new ImmutableBTreeTable[V](columns, minDegree, indexCovers)

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
    ImmutableBTreeTable[V](columns, minDegree, newEntries, indexCovers)
  }

  override def diff(other: ImmutableTable[V]): ImmutableBTreeTable[V] = {
    val newEntries = entries.diff(other.entries)
    ImmutableBTreeTable[V](columns, minDegree, newEntries, indexCovers)
  }

  override def cartesian(other: ImmutableTable[V]): ImmutableBTreeTable[V] = {
    null
  }

  override def project(newCols: Seq[String]): ImmutableBTreeTable[V] = {
    val columnsIndex = indices.head._1.order.zipWithIndex.toMap
    val colsIndex = newCols.map(columnsIndex)
    val projectedIndexCovers = indexCovers.map { cover =>
      IndexCover(cover.order.filter(newCols.contains))
    }
    val newEntries = entries.map { tuple =>
      colsIndex.map(tuple.apply)
    }
    ImmutableBTreeTable[V](newCols, minDegree, newEntries, projectedIndexCovers)
  }

  override def select(f: Tuple => Boolean): ImmutableBTreeTable[V] = {
    val newEntries = entries.filter(f)
    ImmutableBTreeTable[V](cols, minDegree, newEntries, indexCovers)
  }

  override def rename(subst: Map[String, String]): ImmutableBTreeTable[V] = {
    val newTable = new ImmutableBTreeTable[V](cols, minDegree, indexCovers)
    ???
  }

  override def join(other: ImmutableTable[V]): ImmutableBTreeTable[V] = {
    val otherNewCols = other.columns.filter { col =>
      !columns.contains(col)
    }
    val otherNewColsIndices = otherNewCols.map(other.columns.indexOf)
    val newEntries =
      for {
        entry <- entries
        namedEntry = columns.zip(entry).toMap
        otherEntry <- other.entries(namedEntry)
      } yield entry ++ otherNewColsIndices.map(otherEntry.apply)
    ImmutableBTreeTable[V](cols, minDegree, newEntries, indexCovers)
  }

  private def isEquiJoinable(
      t1: NamedTuple,
      t2: NamedTuple
    )(implicit sameCols: Seq[String]
    ): Boolean = sameCols.forall { c =>
    t1(c) == t2(c)
  }

}

object ImmutableBTreeTable {
  def apply[V: ClassTag](
      cols: Seq[String],
      minDegree: Int,
      entries: Seq[Seq[V]],
      indexCovers: Set[IndexCover]
    )(implicit valOrdering: Ordering[V],
      topAndBot: () => (V, V)
    ): ImmutableBTreeTable[V] = {
    val table = new ImmutableBTreeTable[V](cols, minDegree, indexCovers)

    table.indices.foreach { case (indexCover, tree) =>
      val indexOrder = indexCover.toIndexOrder(cols)
      implicit val tupleOrd: Ordering[Seq[V]] = indexOrder.tupleOrdering
      val sortedEntries = entries.sorted
      sortedEntries.foreach(tree.insert)
    }
    table
  }
}
