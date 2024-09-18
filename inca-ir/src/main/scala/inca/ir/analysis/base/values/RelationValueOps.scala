package inca.ir.analysis.base.values

import inca.ir.analysis.RelationOps
import inca.ir.analysis.base.effect.Failure.*
import sturdy.effect.EffectStack
import sturdy.effect.failure.Failure
import sturdy.values.Join
import sturdy.values.booleans.BooleanOps
import sturdy.values.ordering.EqOps

trait RelationValueOps[C, V, B](using effects: EffectStack, joinV: Join[V], booleanOps: BooleanOps[B], eqOps: EqOps[V, B], failure: Failure)
  extends RelationOps[C, V, B, RelationValue[C, V]]:

  type RV = RelationValue[C, V]
  type I[Row] = Option[Row]

  private def joinColumnWise(vals: Seq[Row]) =
    val colwiseVals = vals.map(_.toList).toList.transpose
    colwiseVals.map { col =>
      effects.joinFold(col, identity)
    }

  private def normalize(rv: RV): RV =
    // All relation values Table(X, {()}) should be normalized to Table(E, {()})
    if isEmpty(rv) == booleanOps.boolLit(true) then
      rv
    else
      val allEntries = entries(rv)
      val nonEmptyEntries = allEntries.filter(_.nonEmpty)
      if nonEmptyEntries.isEmpty then
        unit
      else
        RelationValue(columns(rv), nonEmptyEntries)


  def makeColumnName(c: String): C
  def make(cols: Seq[C], vals: Seq[Row]): RV =
      normalize(RelationValue(cols, Some(joinColumnWise(vals))))

  def columns(rv: RV): Seq[C] = rv.cols
  def entries(rv: RV): Option[Row] = rv.rows

  // Unit
  val unit: RV = RelationValue(Seq(), Some(Seq.empty))

  // Empty
  def empty(cols: Seq[C]): RV = make(cols, Seq()) 
  def isEmpty(rv: RV): B = booleanOps.boolLit(entries(rv).isEmpty) // undecideable

  def rename(rv: RV, subst: Map[C, C]): RV =
    val allCols = columns(rv)
    val renamedCols = allCols.map(c => subst.getOrElse(c, c))
    make(renamedCols, entries(rv).toSeq)

  def project(rv: RV, cols: Seq[C]): RV =
    val allCols = columns(rv)
    if isEmpty(rv) == booleanOps.boolLit(true) then
      rv
    else if !cols.exists(allCols.contains) then
      unit
    else
      val projectedColIndices = cols.map(allCols.indexOf).filter(_ >= 0)
      val projectedValues = entries(rv).map(e => projectedColIndices.map(e.apply))
      make(allCols, projectedValues.toSeq)

  def projectAndRename(rv: RV, subst: Map[C, C]): RV =
    val projected = project(rv, subst.keys.toSeq)
    rename(projected, subst)

  def cartesian(rv: RV, other: RV): RV =
    val allCols = columns(rv) ++ columns(other)
    val cartesianValues =
      for (row1 <- entries(rv); row2 <- entries(other))
        yield row1 ++ row2
    make(allCols, cartesianValues.toSeq)

  def select(rv: RV)(f: Row => Boolean): RV =
    val newEntries = entries(rv).filter(f)
    make(columns(rv), newEntries.toSeq)

  def naturalJoin(rv: RV, other: RV): RV =
    if isEmpty(rv) == booleanOps.boolLit(true) then
      rv
    else if isEmpty(other) == booleanOps.boolLit(true) then
      other
    else if rv == unit then
      other
    else if other == unit then
      rv
    else
      val sharedCols = columns(rv).intersect(columns(other))
      val colIndicesRv = sharedCols.map(columns(rv).indexOf).filter(_ > -1)
      val colIndicesOther = sharedCols.map(columns(other).indexOf).filter(_ > -1)
      val combinedCols = columns(rv) ++ columns(other).filterNot(sharedCols.contains)

      val joinedRows =
        for {
          row1 <- entries(rv)
          row2 <- entries(other)
          if colIndicesRv.map(row1.lift) == colIndicesOther.map(row2.lift)
        } yield
          row1 ++ row2.filterNot(colIndicesOther.contains)
      make(combinedCols, joinedRows.toSeq)

  def antiJoin(rv: RV, other: RV): RV =
    val sharedCols = columns(rv).intersect(columns(other))
    val colIndicesRv = sharedCols.map(columns(rv).indexOf)
    val colIndicesOther = sharedCols.map(columns(other).indexOf)
    val filteredRows =
      entries(rv).filter { row1 =>
        !entries(other).exists { row2 =>
          colIndicesRv.map(row1.apply) == colIndicesOther.map(row2.apply)
        }
      }
    make(columns(rv), filteredRows.toSeq)

  def union(rv: RV, other: RV): RV = (rv, other) match
    case (`unit`, _) => other
    case (_, `unit`) => rv
    case _ if columns(rv) != columns(other) =>
      failure(ColumnMismatch, s"Can not union relations with different columns")
    case _ =>
      val combinedEntries = entries(rv) ++ entries(other)
      make(columns(rv), combinedEntries.toSeq)


