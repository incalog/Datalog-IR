package inca.ir.analysis.base.values

import inca.ir.analysis.RelationOps
import sturdy.values.{Changed, Finite, Join, MaybeChanged, Topped, Unchanged, Widen}

import scala.collection

// We can not decide if a table is empty or not.
// Consider we have a body that contains a comparison such as: Top == ConstInt(4)
// This could succeed, but it could also fail.
case class ConstantRelation(cols: Seq[String], rows:Seq[Value], empty: Topped[Boolean])(using joinV: Join[Value]):
  override def toString: String =
    if (rows.isEmpty)
      s"[${cols.mkString(", ")}], $empty"
    else
      s"[${cols.zip(rows).toMap.mkString(", ")}], $empty"

  def rename(subst: Map[String, String]): ConstantRelation =
    val newColumns = cols.map(c => subst.getOrElse(c, c))
    ConstantRelation(newColumns, rows, empty)

  def project(newColumns: Seq[String]): ConstantRelation =
    val colsIndex = newColumns.map(cols.indexOf)
    val newRows = colsIndex.map(rows)
    ConstantRelation(newColumns, newRows, empty)

  def map(columnName: String)(f: Seq[Value] => Value): ConstantRelation =
    ConstantRelation(cols :+ columnName, rows :+ f(rows), empty)

  def flatMap(f: Seq[Value] => ConstantRelation): ConstantRelation = f(rows) match
    case tr@ConstantRelation(_, _, Topped.Actual(true)) =>
      ConstantRelation((cols ++ tr.cols).distinct, Seq(), Topped.Actual(true))
    case tr =>
      naturalJoin(tr)

  def filter(f: Seq[Value] => Topped[Boolean]): ConstantRelation = f(rows) match
    case Topped.Top => copy(empty = Topped.Top)
    case Topped.Actual(true) => this // unchanged
    case Topped.Actual(false) => copy(empty = Topped.Actual(true)) // definitely empty

  def naturalJoin(other: ConstantRelation): ConstantRelation =
    val rvCols = cols.zipWithIndex.toMap
    val otherCols = other.cols.zipWithIndex.toMap

    val newCols = cols ++ other.cols.filterNot(cols.contains)
    val newVals = for (c <- newCols) yield {
      (rvCols.get(c), otherCols.get(c)) match
        case (Some(rvIx), None) => rows(rvIx)
        case (None, Some(otherIx)) => other.rows(otherIx)
        case (Some(rvIx), Some(otherIx)) => joinV(rows(rvIx), other.rows(otherIx)).get
        case (None, None) => throw new IllegalStateException()
    }
    val newEmpty = (empty, other.empty) match
      case (Topped.Actual(true), _) | (_, Topped.Actual(true)) => Topped.Actual(true)
      case _ => Topped.Top
    ConstantRelation(newCols, newVals, newEmpty)

  def antiJoin(other: ConstantRelation): ConstantRelation =
    val newEmpty = other.empty match
      case Topped.Actual(true) => empty
      case _ => Topped.Top
    ConstantRelation(cols, rows, newEmpty)

  def join(other: ConstantRelation): ConstantRelation =
    // TODO: Probably wrong.
    if cols != other.cols then
      throw new IllegalArgumentException("Schemas must match for join")

    val commonRows = rows.zip(other.rows).map { case (v1, v2) => joinV(v1, v2).get }
    val newEmpty = (empty, other.empty) match
      case (Topped.Actual(true), _) | (_, Topped.Actual(true)) => Topped.Actual(true)
      case _ => Topped.Top
    ConstantRelation(cols, commonRows, newEmpty)


class ConstantRelationOps(using joinV: Join[Value]) extends RelationOps[Value, Topped[Boolean], ConstantRelation]:
  override def isEmpty(rv: ConstantRelation): Topped[Boolean] = rv.empty

  override def hasColumn(rv: ConstantRelation, column: String): Boolean = rv.cols.contains(column)

  override def columns(rv: ConstantRelation): Seq[String] = rv.cols

  override def make(cols: Seq[String], vals: Seq[Row]): ConstantRelation =
    val joinedVals = vals.foldLeft[Row](cols.map(_ => Bottom))((v1, v2) => v1.zip(v2).map((t1, t2) => joinV(t1, t2).get))
    ConstantRelation(cols, joinedVals, Topped.Actual(vals.isEmpty))

  override def rename(rv: ConstantRelation, subst: Map[String, String]): ConstantRelation =
    rv.rename(subst)

  override def project(rv: ConstantRelation, newColumns: Seq[String]): ConstantRelation =
    rv.project(newColumns)

  override def map(rv: ConstantRelation, columnName: String)(f: Row => Value): ConstantRelation =
    rv.map(columnName)(f)

  override def flatMap(rv: ConstantRelation)(f: Row => ConstantRelation): ConstantRelation =
    rv.flatMap(f)

  override def filter(rv: ConstantRelation)(f: Row => Topped[Boolean]): ConstantRelation =
    rv.filter(f)

  override def naturalJoin(rv: ConstantRelation, other: ConstantRelation): ConstantRelation =
    rv.naturalJoin(other)

  override def antiJoin(rv: ConstantRelation, other: ConstantRelation): ConstantRelation =
    rv.antiJoin(other)


class JoinRV[Value](using joinValue: Join[Value]) extends Join[ConstantRelation]:
  override def apply(v1: ConstantRelation, v2: ConstantRelation): MaybeChanged[ConstantRelation] =
    MaybeChanged(v1.join(v2), v1)