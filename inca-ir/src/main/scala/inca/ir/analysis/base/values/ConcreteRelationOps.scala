package inca.ir.analysis.base.values

import inca.ir.analysis.RelationOps
import sturdy.effect.failure.Failure
import sturdy.values.ordering.EqOps
import sturdy.values.{Join, MaybeChanged}

import scala.collection

case class ConcreteRelation[V](cols: Seq[String], rows: Set[Seq[V]]):
  def size: Int = rows.size

  //if (cols.nonEmpty && rows.size == 1 && rows.head == Seq())
  //  throw IllegalArgumentException("Unit table must not have columns")

  lazy val isEmpty: Boolean = cols.isEmpty && rows.isEmpty

  def union(other: ConcreteRelation[V]): ConcreteRelation[V] =
    if (cols.toSet != other.cols.toSet)
      throw IllegalArgumentException(s"Not possible to union: $cols <-> ${other.cols}")
    val newEntries =
      if (cols == other.cols) {
        rows ++ other.rows
      } else {
        // Same columns but different order
        val indexMap = cols.map(other.cols.indexOf)
        if (indexMap.contains(-1))
          throw IllegalArgumentException(s"Not possible to union: $cols <-> ${other.cols}")
        def rearrange(entry: Seq[V]): Seq[V] = indexMap.map(entry)
        rows ++ other.rows.map(rearrange)
      }
    ConcreteRelation(cols, newEntries)

  def join(other: ConcreteRelation[V]): ConcreteRelation[V] =
    union(other)
  
  def rename(subst: Map[String, String]): ConcreteRelation[V] =
    val newColumns = cols.map(c => subst.getOrElse(c,c))
    ConcreteRelation(newColumns, rows)

  def project(newColumns: Seq[String]): ConcreteRelation[V] =
    val colsIndex = newColumns.map(cols.indexOf)
    val newRows = rows.map(colsIndex.map)
    ConcreteRelation(newColumns, newRows)

  def cartesian(other: ConcreteRelation[V]): ConcreteRelation[V] =
    if (cols.exists(other.cols.contains))
      throw IllegalArgumentException("Columns need to be disjunct for cartesian product")
    val allCols = cols ++ other.cols
    val cartesianValues =
      for (row1 <- rows; row2 <- other.rows)
        yield row1 ++ row2
    ConcreteRelation(allCols, cartesianValues)

  /**
   * Groups rows by the given groupBy columns and applies an aggregation function
   * to compute new columns based on the accumulator columns.
   *
   * Example:
   *
   * Original table:
   * +------------+----------+-----+
   * | date       | category | value |
   * +------------+----------+-----+
   * | 2025-02-28 | food     | 10  |
   * | 2025-02-28 | food     | 15  |
   * | 2025-02-28 | transport| 5   |
   * | 2025-02-27 | food     | 20  |
   * +------------+----------+-----+
   *
   * Function call:
   * groupBy(
   *  accumulatorCols = Seq("value"),
   *  groupByCols = Seq("date", "category")
   * )(
   *  newCols = Seq("date", "category", "sumValue"),
   *  (groupedValues, accValues) => groupedValues ++ Seq(accValues.flatten.sum)
   * )
   *
   * Explanation:
   * - groupByCols: Seq("date", "category")
   * - accumulatorCols: Seq("value")
   * - newCols: Seq("date", "category", "sumValue")
   * - Aggregation: Sum the `value` column for each (date, category) group.
   *
   * After grouping:
   * +------------+----------+------------+
   * | date       | category | values     |
   * +------------+----------+------------+
   * | 2025-02-28 | food     | [10, 15]   |
   * | 2025-02-28 | transport| [5]        |
   * | 2025-02-27 | food     | [20]       |
   * +------------+----------+------------+
   *
   * After applying f:
   * +------------+----------+---------+
   * | date       | category | sumValue |
   * +------------+----------+---------+
   * | 2025-02-28 | food     | 25      |
   * | 2025-02-28 | transport| 5       |
   * | 2025-02-27 | food     | 20      |
   * +------------+----------+---------+
   *
   * Result:
   * ConcreteRelation(
   *  cols = Seq("date", "category", "sumValue"),
   *  rows = Set(
   *   Seq("2025-02-28", "food", 25),
   *   Seq("2025-02-28", "transport", 5),
   *   Seq("2025-02-27", "food", 20)
   *  )
   * )
   *
   * @param accumulatorCols Columns to aggregate (e.g., ["value"])
   * @param groupByCols     Columns to group by (e.g., ["date", "category"])
   * @param newCols         Columns in the resulting table
   * @param f               Aggregation function applied to each group
   * @return New ConcreteRelation with grouped and aggregated data
   */
  def groupBy(accumulatorCols: Seq[String], groupByCols: Seq[String])
             (newCols: Seq[String], f: (groupByValues: Seq[V], accValues: Seq[Seq[V]]) => Seq[V]): ConcreteRelation[V] =
    val groupByIndices = groupByCols.map(cols.indexOf)
    val accIndices = accumulatorCols.map(cols.indexOf)
    val grouped = rows.groupBy(row => groupByIndices.map(row.apply))
    val newRows = grouped.map { (groupedValues, rows) =>
      val accValues = rows.toSeq.map(row => accIndices.map(row.apply))
      val newRow = f(groupedValues, accValues)
      if (newRow.size != newCols.size)
        throw IllegalStateException("Number of new columns must match arity of new rows.")
      newRow
    }
    ConcreteRelation(newCols, newRows.toSet)

  def filter(f: Seq[V] => Boolean): ConcreteRelation[V] =
    ConcreteRelation(cols, rows.filter(f))
  
  def map(columnName: String)(f: Seq[V] => V): ConcreteRelation[V] =
    ConcreteRelation(cols :+ columnName, rows.map(r => r :+ f(r)))

  def naturalJoin(other: ConcreteRelation[V]): ConcreteRelation[V] =
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
    ConcreteRelation(cols ++ otherNewCols, newEntries)

  def antiJoin(other: ConcreteRelation[V]): ConcreteRelation[V] =
    if (isEmpty || other.isEmpty)
      this
    else
      val sharedCols = cols.intersect(other.cols)
      if (sharedCols.isEmpty)
        throw IllegalArgumentException(s"Not possible to anti join with disjunct columns: $cols <-> ${other.cols}")
      val sameColsIndices = sharedCols.map(cols.indexOf)
      val sameOtherColsIndices = sharedCols.map(other.cols.indexOf)
      val filteredRows = rows.filter { row1 =>
        !other.rows.exists { row2 =>
          sameColsIndices.map(row1.apply) == sameOtherColsIndices.map(row2.apply)
        }
      }
      ConcreteRelation(cols,  filteredRows)

class ConcreteRelationOps[V](using failure: Failure, eqOps: EqOps[V, Boolean])
  extends RelationOps[V, Boolean, ConcreteRelation[V]]:

  type RV = ConcreteRelation[V]

  override def isEmpty(rv: ConcreteRelation[V]): Boolean = rv.rows.isEmpty

  override def hasColumn(rv: RV, column: String): Boolean = rv.cols.contains(column)

  override def columns(rv: RV): Seq[String] = rv.cols

  override def make(cols: Seq[String], vals: Seq[Row]): ConcreteRelation[V] = ConcreteRelation(cols, vals.toSet)

  override def rename(rv: ConcreteRelation[V], subst: Map[String, String]): ConcreteRelation[V] =
    rv.rename(subst)

  override def project(rv: ConcreteRelation[V], cols: Seq[String]): ConcreteRelation[V] =
    rv.project(cols)

  override def extract(rv: ConcreteRelation[V], columnNames: Seq[String]): Seq[Row] =
    val colIndices = columnNames.map(rv.cols.indexOf)
    rv.rows.map(row => colIndices.map(row)).toSeq

  override def filter(rv: ConcreteRelation[V])(f: Row => Boolean): ConcreteRelation[V] =
    rv.filter(f)

  override def filter(rv: ConcreteRelation[V])(f: Row => Boolean)(refine: Row => Row): ConcreteRelation[V] =
    // we are precise, we don't need refinement
    filter(rv)(f)

  override def filterEq(rv: RV, col: String, col2: String): RV =
    val lix = columnIndex(rv, col)
    val rix = columnIndex(rv, col2)
    filter(rv)(row => eqOps.equ(row(lix), row(rix)))
  
  override def filterNeq(rv: RV, col: String, col2: String): RV =
    val lix = columnIndex(rv, col)
    val rix = columnIndex(rv, col2)
    filter(rv)(row => eqOps.neq(row(lix), row(rix)))

  override def groupBy(rv: RV, accumulatorCols: Seq[String], groupByCols: Seq[String])
                      (newCols: Seq[String], f: (groupByValues: Row, accValues: Seq[Row]) => Row): RV =
    rv.groupBy(accumulatorCols, groupByCols)(newCols, f)

  override def map(rv: ConcreteRelation[V], columnName: String)(f: Row => V): ConcreteRelation[V] =
    rv.map(columnName)(f)

  private def repeat[A](v: A, n: Int) = (1 to n).map(_ => v)

  override def projectAndRenameWithMultipleAliases(rv: RV, subst: Map[String, Seq[String]]): RV =
    val rows = rv.rows.toSeq
    val initialRows = repeat(Seq[V](), rv.rows.size)
    val (newCols, newRows) = subst.foldLeft((Seq[String](), initialRows)) {
      case ((accCols, accRows), (supColumn, cols)) =>
        val cIx = columnIndex(rv, supColumn)
        val dupRows = rows.map { r =>
          if (r.isEmpty)
            r
          else
            repeat(r(cIx), cols.size)
        }
        (accCols ++ cols, accRows.zip(dupRows).map((r1, r2) => r1 ++ r2))
    }
    ConcreteRelation(newCols, newRows.toSet)

  override def flatMap(rv: ConcreteRelation[V])(f: Row => ConcreteRelation[V]): ConcreteRelation[V] =
    assert(!rv.isEmpty)
    val generated = rv.rows.map(f).reduce(_.union(_))
    rv.naturalJoin(generated)

  override def naturalJoin(rv: ConcreteRelation[V], other: ConcreteRelation[V]): ConcreteRelation[V] =
    rv.naturalJoin(other)

  override def antiJoin(rv: ConcreteRelation[V], other: ConcreteRelation[V]): ConcreteRelation[V] =
    rv.antiJoin(other)

given JoinCRV[V]: Join[ConcreteRelation[V]] with {
  override def apply(v1: ConcreteRelation[V], v2: ConcreteRelation[V]): MaybeChanged[ConcreteRelation[V]] =
    MaybeChanged(v1.join(v2), v1)
}