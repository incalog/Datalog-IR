package inca.ir.analysis.base.values

import inca.ir.analysis.RelationOps
import sturdy.values.{Changed, Finite, Join, MaybeChanged, Unchanged, Widen}

import scala.collection

case class RelationValue[C, V](cols: Seq[C], rows: Option[Seq[V]]):
  def size: Int = rows.size

  def isUnit: Boolean = cols.isEmpty && (rows.size == 1) && rows.head.isEmpty
  def isEmpty: Boolean = rows.isEmpty

class FiniteRV[C, V] extends Finite[RelationValue[C, V]]

class JoinRV[C, V](using joinValue: Join[V]) extends Join[RelationValue[C, V]]:

  private def join(v1: RelationValue[C, V], v2: RelationValue[C, V]): RelationValue[C, V] =
    // top: RelationValue(X, Seq(Seq(Top, ..., Top)))
    // bot: empty table

    // When we are in a fixpoint then the result needs to stabilize when we encounter an empty table.
    if v1.isEmpty then
      v1
    else if v2.isEmpty then
      v2
    else
      val sharedCols = v1.cols.intersect(v2.cols)
      val colIndicesRv = sharedCols.map(v1.cols.indexOf).filter(_ > -1)
      val colIndicesOther = sharedCols.map(v2.cols.indexOf).filter(_ > -1)
      val combinedCols = sharedCols
                          ++ v1.cols.filterNot(sharedCols.contains)
                          ++ v2.cols.filterNot(sharedCols.contains)
      // Join all columns that v1 and v2 have in common. Keep the rest unchanged
      val joinedRows =
        for {
          row1 <- v1.rows
          row2 <- v2.rows
        } yield
          val sharedRow1 = colIndicesRv.map(row1.apply)
          val sharedRow2 = colIndicesOther.map(row1.apply)
          val sharedRow = sharedRow1.zip(sharedRow2).map(joinValue.apply).map(_.get)
          sharedRow
            ++ row1.filterNot(colIndicesRv.contains)
            ++ row2.filterNot(colIndicesOther.contains)

      val (sortedCols, sortedRows) = combinedCols.zip(joinedRows.get).sortBy((col, _) => col.toString).unzip

      RelationValue(sortedCols, Some(sortedRows))

  override def apply(v1: RelationValue[C, V], v2: RelationValue[C, V]): MaybeChanged[RelationValue[C, V]] =
    val joined = join(v1, v2)
    if joined == v1 then
      Unchanged(joined)
    else
      Changed(joined)