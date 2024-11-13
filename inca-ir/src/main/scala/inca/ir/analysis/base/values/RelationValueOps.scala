package inca.ir.analysis.base.values

import inca.ir.analysis.RelationOps
import inca.ir.analysis.base.effect.*
import sturdy.effect.EffectStack
import sturdy.effect.failure.Failure
import sturdy.values.Join
import sturdy.values.booleans.BooleanOps
import sturdy.values.ordering.EqOps

trait RelationValueOps[V, B](using effects: EffectStack, joinV: Join[V], booleanOps: BooleanOps[B], eqOps: EqOps[V, B], failure: Failure)
  extends RelationOps[V, B, RelationValue[V]]:

  type RV = RelationValue[V]
  type I[Row] = Option[Row]

  private def joinColumnWise(vals: Seq[Row]) =
    val colwiseVals = vals.map(_.toList).toList.transpose
    colwiseVals.map { col =>
      effects.joinFold(col, identity)
    }

  def make(cols: Seq[String], vals: Seq[Row]): RV =
    RelationValue(cols, Some(joinColumnWise(vals)))
  //normalize(RelationValue(cols, Some(joinColumnWise(vals))))

  def columns(rv: RV): Seq[String] = rv.cols

  def entries(rv: RV): Option[Row] = rv.rows

  // Unit
  //val unit: RV = RelationValue(Seq(), Some(Seq.empty))

  def rename(rv: RV, subst: Map[String, String]): RV =
    val allCols = columns(rv)
    val renamedCols = allCols.map(c => subst.getOrElse(c, c))
    make(renamedCols, entries(rv).toSeq)

  def project(rv: RV, cols: Seq[String]): RV =
    val allCols = columns(rv)
    val projectedColIndices = cols.map(allCols.indexOf).filter(_ >= 0)
    val projectedValues = entries(rv).map(e => projectedColIndices.map(e.apply))
    make(allCols, projectedValues.toSeq)

  def projectAndRename(rv: RV, subst: Map[String, String]): RV =
    val projected = project(rv, subst.keys.toSeq)
    rename(projected, subst)

  def cartesian(rv: RV, other: RV): RV =
    val allCols = columns(rv) ++ columns(other)
    val cartesianValues =
      for (row1 <- entries(rv); row2 <- entries(other))
        yield row1 ++ row2
    make(allCols, cartesianValues.toSeq)

  def filter(rv: RV)(f: Row => B): RV =
    val newEntries = entries(rv).filter(r => booleanOps.boolLit(true) == f(r))
    make(columns(rv), newEntries.toSeq)

  def map[A](rv: RV)(f: Row => A): I[A] =
    entries(rv).map(f)

  def naturalJoin(rv: RV, other: RV): RV =
    val rvIsUnit = entries(rv).head.isEmpty
    val otherIsUnit = entries(other).head.isEmpty
    if (rvIsUnit)
      other
    else if (otherIsUnit)
      rv
    else
      val sharedCols = columns(rv).intersect(columns(other))
      val colIndicesRv = sharedCols.map(columns(rv).indexOf).filter(_ > -1)
      val colIndicesOther = sharedCols.map(columns(other).indexOf).filter(_ > -1)
      val combinedCols = columns(rv) ++ columns(other).filterNot(sharedCols.contains)

      val joinedRows =
        (for {
          row1 <- entries(rv)
          row2 <- entries(other)
        } yield
          row1 ++ row2.zipWithIndex.filterNot { case (_, idx) => colIndicesOther.contains(idx) }.map(_._1)).toSeq

      // sanity check
      if (joinedRows.isEmpty || (joinedRows.head.nonEmpty && combinedCols.size != joinedRows.head.size))
        throw IllegalStateException(s"Can not natural join values: $rv - $other")

      make(combinedCols, joinedRows)

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
    case _ if columns(rv) != columns(other) =>
      failure(ColumnMismatch, s"Can not union relations with different columns")
    case _ =>
      val combinedEntries = entries(rv) ++ entries(other)
      make(columns(rv), combinedEntries.toSeq)


