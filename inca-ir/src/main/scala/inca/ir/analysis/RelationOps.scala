package inca.ir.analysis

trait RelationOps[V, B, RV]:
  type Row = Seq[V]

  def isEmpty(rv: RV): B

  def hasColumn(rv: RV, column: String): Boolean

  def columns(rv: RV): Seq[String]

  def columnIndex(rv: RV, column: String): Int =
    columns(rv).indexOf(column)

  /** may produce empty table */
  def make(cols: Seq[String], vals: Seq[Row]): RV
  
  def rename(rv: RV, subst: Map[String, String]): RV

  def project(rv: RV, cols: Seq[String]): RV

  def projectAndRename(rv: RV, subst: Map[String, String]): RV =
    rename(project(rv, subst.keys.toSeq), subst)

  def projectAndRenameWithMultipleAliases(rv: RV, subst: Map[String, Seq[String]]): RV
  
  def map(rv: RV, columnName: String)(f: Row => V): RV

  /** may produce empty table (but only if you use groupByCols that don't exist in rv) */
  def groupBy(rv: RV, accumulatorCols: Seq[String], groupByCols: Seq[String])
             (newCols: Seq[String], f: (groupByValues: Row, accValues: Seq[Row]) => Row): RV

  def groupBy(rv: RV, accumulatorCol: String, groupByCols: Seq[String])
             (newCols: Seq[String], f: (groupByValues: Row, accValues: Seq[V]) => Row): RV =
    groupBy(rv, Seq(accumulatorCol), groupByCols)(newCols, { (groupedValues, accValues) =>
      f(groupedValues, accValues.flatten)
    })

  def fold(rv: RV, initial: Row)(f: (Row, Row) => Row): RV

  /** may produce empty table */
  def flatMap(rv: RV)(f: Row => RV): RV

  /** may produce empty table */
  def filter(rv: RV)(f: Row => B): RV

  /** may produce empty table */
  def filterEq(rv: RV, col: String, col2: String): RV

  /** may produce empty table */
  def filterNeq(rv: RV, col: String, col2: String): RV

  /** may produce empty table */
  def naturalJoin(rv: RV, other: RV): RV

  /** may produce empty table */
  def antiJoin(rv: RV, other: RV): RV
  
  def copyColumn(rv: RV, from: String, to: String): RV =
    val fromIx = columnIndex(rv, from)
    map(rv, to) { row => row(fromIx) }
    