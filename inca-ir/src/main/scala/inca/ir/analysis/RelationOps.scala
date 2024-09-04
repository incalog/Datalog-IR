package inca.ir.analysis

import sturdy.values.booleans.BooleanOps

trait RelationOps[C, V, B, RV](using booleanOps: BooleanOps[B]):
  type I[T] = Iterable[T]
  type Row = Seq[V]

  def makeColumnName(c: String): C
  def embedRows(r: Row*): I[Row]

  // Unit
  def make(cols: Seq[C], vals: I[Row], neg: Boolean = false): RV
  def unit: RV = make(Seq(), embedRows(Seq.empty))
  //def isUnit(rv: RV): B =
  //  val values = entries(rv)
  //  booleanOps.boolLit((values.size == 1) && (values.head == Seq.empty))

  // Empty
  def empty(cols: Seq[C]): RV = make(cols, embedRows())
  def isEmpty(rv: RV): B = booleanOps.boolLit(entries(rv).isEmpty)
  def nonEmpty(rv: RV): B = booleanOps.boolLit(entries(rv).nonEmpty)

  def rename(rv: RV, subst: Map[C, C]): RV =
    val allCols = columns(rv)
    val renamedCols = allCols.map(c => subst.getOrElse(c, c))
    make(renamedCols, entries(rv))

  def project(rv: RV, cols: Seq[C]): RV =
    val allCols = columns(rv)
    if !cols.exists(allCols.contains) then
      unit
    else
      val projectedColIndices = cols.map(allCols.indexOf).filter(_ >= 0)
      val projectedValues = entries(rv).map(e => projectedColIndices.map(e.apply))
      make(allCols, projectedValues)

  def projectAndRename(rv: RV, subst: Map[C, C]): RV =
    val projected = project(rv, subst.keys.toSeq)
    rename(projected, subst)
    
  def markNegative(rv: RV): RV =
    val cols = columns(rv)
    val values = entries(rv)
    make(cols, values, true)


  def columns(rv: RV): Seq[C]
  def entries(rv: RV): I[Row]



  // TODO: Are these needed
  def size(rv: RV): Int = columns(rv).size
  def columnIndex(rv: RV, column: C): Int
  def namedEntries(rv: RV): Map[C, Row]

  def contains(rv: RV, nv: (C, V)): Boolean
  def subset(rv: RV, other: RV): Boolean

  def union(rv: RV, other: RV): RV
  def diff(rv: RV, other: RV): RV
  def cartesian(rv: RV, other: RV): RV
  def select(rv: RV, f: Tuple => Boolean): RV
  def naturalJoin(rv: RV, other: RV): RV
  def antiJoin(rv: RV, other: RV): RV