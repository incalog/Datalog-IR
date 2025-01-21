package inca.ir.analysis.base.values

import inca.ir.analysis.RelationOps
import sturdy.values.booleans.BooleanOps
import sturdy.values.ordering.EqOps
import sturdy.values.{Join, MaybeChanged, Topped}

import scala.collection

// We can not decide if a table is empty or not.
// Consider we have a body that contains a comparison such as: Top == ConstantIntV(4)
// This could succeed, but it could also fail.
case class ConstantRelation(cols: Seq[String], rows:Seq[Value], empty: Topped[Boolean]):
  override def toString: String =
    if (rows.isEmpty)
      s"[${cols.mkString(", ")}, $empty]"
    else
      s"[${cols.zip(rows).toMap.mkString(", ")}, $empty]"

class ConstantRelationOps(using joinV: Join[Value], boolOps: BooleanOps[Topped[Boolean]], eqOps: EqOps[Value, Topped[Boolean]]) extends RelationOps[Value, Topped[Boolean], ConstantRelation]:
  override def isEmpty(rv: ConstantRelation): Topped[Boolean] = rv.empty

  override def hasColumn(rv: ConstantRelation, column: String): Boolean = rv.cols.contains(column)

  override def columns(rv: ConstantRelation): Seq[String] = rv.cols

  override def make(cols: Seq[String], vals: Seq[Row]): ConstantRelation =
    val joinedVals = vals.foldLeft[Row](cols.map(_ => Bottom))((v1, v2) => v1.zip(v2).map((t1, t2) => joinV(t1, t2).get))
    ConstantRelation(cols, joinedVals, Topped.Actual(vals.isEmpty))

  override def rename(rv: ConstantRelation, subst: Map[String, String]): ConstantRelation =
    val newColumns = rv.cols.map(c => subst.getOrElse(c, c))
    ConstantRelation(newColumns, rv.rows, rv.empty)

  override def project(rv: ConstantRelation, newColumns: Seq[String]): ConstantRelation =
    val colsIndex = newColumns.map(rv.cols.indexOf)
    val newRows = colsIndex.map(rv.rows)
    ConstantRelation(newColumns, newRows, rv.empty)

  override def map(rv: ConstantRelation, columnName: String)(f: Seq[Value] => Value): ConstantRelation =
    ConstantRelation(rv.cols :+ columnName, rv.rows :+ f(rv.rows), rv.empty)

  override def fold(rv: ConstantRelation, initial: Row)(f: (Row, Row) => Row): ConstantRelation =
    ConstantRelation(rv.cols, f(initial, rv.rows), empty = Topped.Actual(false))
  
  override def flatMap(rv: ConstantRelation)(f: Seq[Value] => ConstantRelation): ConstantRelation =
    naturalJoin(rv, f(rv.rows))

  override def filter(rv: ConstantRelation)(f: Seq[Value] => Topped[Boolean]): ConstantRelation = f(rv.rows) match
    case Topped.Top => rv.copy(empty = Topped.Top)
    case Topped.Actual(true) => rv // unchanged
    case Topped.Actual(false) => rv.copy(empty = Topped.Actual(true)) // definitely empty

  override def naturalJoin(rv: ConstantRelation, other: ConstantRelation): ConstantRelation =
    val rvCols = rv.cols.zipWithIndex.toMap
    val otherCols = other.cols.zipWithIndex.toMap

    val newCols = rv.cols ++ other.cols.filterNot(rv.cols.contains)
    var newEmpty = (rv.empty, other.empty) match
      case (Topped.Actual(true), _) | (_, Topped.Actual(true)) => Topped.Actual(true) // definitely empty
      case (Topped.Top, _) | (_, Topped.Top) => Topped.Top // we don't know
      case _ => Topped.Actual(false) // we need to refine the result

    val newVals = for (c <- newCols) yield {
      (rvCols.get(c), otherCols.get(c)) match
        case (Some(rvIx), None) => rv.rows(rvIx)
        case (None, Some(otherIx)) => other.rows(otherIx)
        case (Some(rvIx), Some(otherIx)) =>
          // If both entries are constants, we can decide if the join succeeds
          val compare = eqOps.equ(rv.rows(rvIx), other.rows(otherIx))
          newEmpty = compare match
            case Topped.Actual(b) => boolOps.or(newEmpty, Topped.Actual(!b))
            case _ => Topped.Top //boolOps.or(newEmpty, compare)
          joinV(rv.rows(rvIx), other.rows(otherIx)).get
        case (None, None) => throw new IllegalStateException()
    }
    ConstantRelation(newCols, newVals, newEmpty)


  override def antiJoin(rv: ConstantRelation, other: ConstantRelation): ConstantRelation =
    val sharedCols = rv.cols.intersect(other.cols)
    if (sharedCols.isEmpty)
      throw IllegalArgumentException(s"Not possible to anti join with disjunct columns: ${rv.cols} <-> ${other.cols}")
    val (newRows, newEmpty) = (rv.empty, other.empty) match
      case (Topped.Actual(true), Topped.Actual(true)) => (Seq(), Topped.Actual(true))
      case (Topped.Actual(false), Topped.Actual(true)) => (rv.rows, Topped.Actual(false))
      case (Topped.Actual(false), Topped.Actual(false)) =>
        val sameColsIndices = sharedCols.map(rv.cols.indexOf)
        val sameOtherColsIndices = sharedCols.map(other.cols.indexOf)
        val comparison = sameColsIndices.zip(sameOtherColsIndices).map { (rvIx, oIx) =>
          eqOps.equ(rv.rows(rvIx), other.rows(oIx))
        }
        val isEmpty = comparison.foldLeft(Topped.Actual(true))((acc, b) => boolOps.and(acc, b))
        isEmpty match
          case Topped.Actual(true) => (Seq(), isEmpty)
          case _ => (rv.rows, isEmpty)
      case _ => (rv.rows, Topped.Top)
    ConstantRelation(rv.cols, newRows, newEmpty)

given JoinRV(using joinV: Join[Value], boolOps: BooleanOps[Topped[Boolean]], eqOps: EqOps[Value, Topped[Boolean]]): Join[ConstantRelation] with {
  def join(rv: ConstantRelation, other: ConstantRelation): ConstantRelation =
    if (rv.cols.toSet != other.cols.toSet)
      throw new IllegalArgumentException("Schemas must match for join")
    val others2Rows = other.cols.map(rv.cols.indexOf)

    val newEmpty = boolOps.and(rv.empty, other.empty)
    if (newEmpty.isActual && newEmpty.get)
      // Empty tables should not have bindings
      ConstantRelation(rv.cols, rv.cols.map(_ => Bottom), newEmpty)
    else
      val newRows = rv.rows.zip(others2Rows.map(other.rows.apply)).map { (v1, v2) => joinV(v1, v2).get }
      ConstantRelation(rv.cols, newRows, newEmpty)

  override def apply(v1: ConstantRelation, v2: ConstantRelation): MaybeChanged[ConstantRelation] =
    MaybeChanged(join(v1, v2), v1)
}