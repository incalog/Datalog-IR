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
    // bot: Exception
    if (v1.rows.head.isEmpty) {
      v2
    } else if (v2.rows.head.isEmpty) {
      v1
    } else if (v1.cols.size != v2.cols.size) {
      throw IllegalStateException(s"Can not join unequal relation values: ${v1} : ${v2}")
    } else {
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
          val sharedRow = sharedRow1.zip(sharedRow2).map((v1, v2) => joinValue(v1, v2)).map(_.get)
          sharedRow
            ++ row1.filterNot(colIndicesRv.contains)
            ++ row2.filterNot(colIndicesOther.contains)

      val (sortedCols, sortedRows) = combinedCols.zip(joinedRows.get).sortBy((col, _) => col.toString).unzip

      RelationValue(sortedCols, Some(sortedRows))
    }

  override def apply(v1: RelationValue[V], v2: RelationValue[V]): MaybeChanged[RelationValue[V]] =
    val joined = join(v1, v2)
    if joined == v1 then
      Unchanged(joined)
    else
      Changed(joined)