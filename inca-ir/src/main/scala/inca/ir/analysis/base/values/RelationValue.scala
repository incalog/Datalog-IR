package inca.ir.analysis.base.values

import inca.ir.analysis.RelationOps
import sturdy.values.{Changed, Finite, Join, MaybeChanged, Unchanged, Widen}

import scala.collection

// Schema: Map[C -> V]
// Special cases (rows):
case class RelationValue[V](cols: Seq[String], rows: Option[Seq[V]]):
  def size: Int = rows.size

class FiniteRV[C, V] extends Finite[RelationValue[V]]

class JoinRV[C, V](using joinValue: Join[V]) extends Join[RelationValue[V]]:

  private def join(v1: RelationValue[V], v2: RelationValue[V]): RelationValue[V] =
    // top: RelationValue(X, Seq(Seq(Top, ..., Top)))
    // bot

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

  override def apply(v1: RelationValue[V], v2: RelationValue[V]): MaybeChanged[RelationValue[V]] =
    val joined = join(v1, v2)
    if joined == v1 then
      Unchanged(joined)
    else
      Changed(joined)