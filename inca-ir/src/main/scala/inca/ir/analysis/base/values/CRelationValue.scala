package inca.ir.analysis.base.values

import inca.ir.analysis.RelationOps
import sturdy.effect.failure.ConcreteFailure
import sturdy.values.{Changed, Finite, Join, MaybeChanged, Unchanged, Widen}

import scala.collection

case class CRelationValue[V](cols: Seq[String], rows: Set[Seq[V]]):
  def size: Int = rows.size

  lazy val isUnit: Boolean = cols.isEmpty && rows.size == 1 && rows.head == Seq()

  def union(other: CRelationValue[V]): CRelationValue[V] =
    if (isUnit)
      other
    else if (other.isUnit)
      this
    else if (cols.size != other.cols.size)
      throw IllegalArgumentException(s"Not possible to union: $cols <-> ${other.cols}")
    else
      val newEntries =
        if (other.cols == other.cols) {
          rows ++ other.rows
        } else {
          val indexMap = cols.map(other.cols.indexOf)
          def rearrange(entry: Seq[V]): Seq[V] = indexMap.map(entry)
          rows ++ other.rows.map(rearrange)
        }
      CRelationValue(cols, newEntries)

  def rename(subst: Map[String, String]): CRelationValue[V] =
    val newColumns = cols.map(c => subst.getOrElse(c,c))
    CRelationValue(newColumns, rows)

  def project(newColumns: Seq[String]): CRelationValue[V] =
    val colsIndex = newColumns.map(cols.indexOf)
    val newRows = rows.map(colsIndex.map)
    CRelationValue(newColumns, newRows)

  def projectAndRename(subst: Map[String, String]): CRelationValue[V] =
    project(subst.keys.toSeq).rename(subst)

  def cartesian(other: CRelationValue[V]): CRelationValue[V] =
    if (isUnit)
      other
    else if (other.isUnit)
      this
    else if (cols.exists(c => other.cols.contains(c)))
      throw IllegalArgumentException("Columns need to be disjunct for cartesian product")
    else
      val allCols = cols ++ other.cols
      val cartesianValues =
        for (row1 <- rows; row2 <- other.rows)
          yield row1 ++ row2
      CRelationValue(allCols, cartesianValues)

  def filter(f: Seq[V] => Boolean): CRelationValue[V] =
    CRelationValue(cols, rows.filter(f))

  def map(columnName: String)(f: Seq[V] => V): CRelationValue[V] =
    CRelationValue(cols :+ columnName, rows.map(r => r :+ f(r)))

  def naturalJoin(other: CRelationValue[V]): CRelationValue[V] =
    if (isUnit)
      other
    else if (other.isUnit)
      this
    else
      val (sameCols, otherNewCols) = other.cols.partition(cols.contains)
      val otherNewColsIndices = otherNewCols.map(other.cols.indexOf)
      val newEntries =
        if (sameCols.isEmpty) {
          // this is a cartesian product
          // we cannot apply an efficient join technique
          for (row1 <- rows; row2 <- other.rows)
            yield row1 ++ row2
        } else {
          for {
            row <- rows
            otherRow <- other.rows
            sameNamedEntries = cols.zip(row).filter { case (k, _) => sameCols.contains(k) }
            (otherSameNamedEntry, otherNewNamedEntries) = other.cols.zip(otherRow).partition {
              case (k, e) => sameCols.contains(k)
            }
            if sameNamedEntries == otherSameNamedEntry
          } yield {
            row ++ otherNewNamedEntries.map(_._2)
          }
        }
      CRelationValue(cols ++ otherNewCols, newEntries)


given JoinCRV[V]: Join[CRelationValue[V]] with {
  override def apply(v1: CRelationValue[V], v2: CRelationValue[V]): MaybeChanged[CRelationValue[V]] =
    MaybeChanged(v1.union(v2), v1)
}