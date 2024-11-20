package inca.ir.analysis.base.values

import sturdy.values.{Join, MaybeChanged}

import scala.collection

case class CRelationValue[V](cols: Seq[String], rows: Set[Seq[V]]):
  def size: Int = rows.size

  if (cols.nonEmpty && rows.size == 1 && rows.head == Seq())
    throw IllegalArgumentException("Unit table must not have columns")

  lazy val isUnit: Boolean = cols.isEmpty && rows.size == 1 && rows.head == Seq()

  def union(other: CRelationValue[V]): CRelationValue[V] =
    if (isUnit)
      other
    else if (other.isUnit)
      this
    else
      if (cols.size != other.cols.size)
        throw IllegalArgumentException(s"Not possible to union: $cols <-> ${other.cols}")
      val newEntries =
        if (cols == other.cols) {
          rows ++ other.rows
        } else {
          val indexMap = cols.map(other.cols.indexOf)
          if (indexMap.contains(-1))
            throw IllegalArgumentException(s"Not possible to union: $cols <-> ${other.cols}")
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
    if (cols.exists(other.cols.contains))
      throw IllegalArgumentException("Columns need to be disjunct for cartesian product")
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
    val (sameCols, otherNewCols) = other.cols.partition(cols.contains)
    val newEntries =
      val sameColsIndices = sameCols.map(cols.indexOf)
      val sameOtherColsIndices = sameCols.map(other.cols.indexOf)
      val newOtherColsIndices = otherNewCols.map(other.cols.indexOf)

      for {
        row <- rows
        otherRow <- other.rows
        if sameColsIndices.map(row) == sameOtherColsIndices.map(otherRow)
      } yield {
        row ++ newOtherColsIndices.map(otherRow)
      }
    CRelationValue(cols ++ otherNewCols, newEntries)

  def antiJoin(other: CRelationValue[V]): CRelationValue[V] =
    if (isUnit)
      other
    else if (other.isUnit)
      this
    else
      val sharedCols = cols.intersect(other.cols)
      val sameColsIndices = sharedCols.map(cols.indexOf)
      val sameOtherColsIndices = sharedCols.map(other.cols.indexOf)
      val filteredRows = rows.filter { row1 =>
          !other.rows.exists { row2 =>
            sameColsIndices.map(row1.apply) == sameOtherColsIndices.map(row2.apply)
          }
        }
      CRelationValue(cols,  filteredRows)

given JoinCRV[V]: Join[CRelationValue[V]] with {
  override def apply(v1: CRelationValue[V], v2: CRelationValue[V]): MaybeChanged[CRelationValue[V]] =
    MaybeChanged(v1.union(v2), v1)
}