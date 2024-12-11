package inca.ir.analysis.base.values

import inca.ir.analysis.RelationOps
import sturdy.values.ordering.EqOps
import sturdy.values.{Changed, Finite, Join, MaybeChanged, Topped, Unchanged, Widen}

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

class ConstantRelationOps(using joinV: Join[Value], meetV: BaseMeetV, eqOps: EqOps[Value, Topped[Boolean]]) extends RelationOps[Value, Topped[Boolean], ConstantRelation]:
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

  override def flatMap(rv: ConstantRelation)(f: Seq[Value] => ConstantRelation): ConstantRelation =
    naturalJoin(rv, f(rv.rows))

  override def filter(rv: ConstantRelation)(f: Seq[Value] => Topped[Boolean]): ConstantRelation = f(rv.rows) match
    case Topped.Top => rv.copy(empty = Topped.Top)
    case Topped.Actual(true) => rv // unchanged
    case Topped.Actual(false) => rv.copy(empty = Topped.Actual(true)) // definitely empty

  override def naturalJoin(rv: ConstantRelation, other: ConstantRelation): ConstantRelation =
    def or(t1: Topped[Boolean], t2: Topped[Boolean]) = (t1, t2) match
      case (Topped.Top, _) | (_, Topped.Top) => Topped.Top
      case (Topped.Actual(false), _) => t2
      case (_, Topped.Actual(false)) => t1
      case (Topped.Actual(true), _) => t1

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
            case Topped.Actual(b) => or(newEmpty, Topped.Actual(!b))
            case _ => or(newEmpty, compare)
          meetV.meet(rv.rows(rvIx), other.rows(otherIx))
        case (None, None) => throw new IllegalStateException()
    }
    ConstantRelation(newCols, newVals, newEmpty)

  override def antiJoin(rv: ConstantRelation, other: ConstantRelation): ConstantRelation =
    // TODO: We can actually get a more precise result here

    val newEmpty = other.empty match
      case Topped.Actual(true) => rv.empty
      case _ => Topped.Top
    val r = ConstantRelation(rv.cols, rv.rows, newEmpty)
    println(s"Anti Join: $rv -- $other :: $r")
    r


class JoinRV(using joinV: Join[Value]) extends Join[ConstantRelation]:
  def join(rv: ConstantRelation, other: ConstantRelation): ConstantRelation =
    if (rv.cols != other.cols)
      throw new IllegalArgumentException("Schemas must match for join")
    val newRows = rv.rows.zip(other.rows).map((v1, v2) => joinV(v1, v2).get)

    val newEmpty = (rv.empty, other.empty) match
      case (Topped.Actual(true), Topped.Actual(true)) => Topped.Actual(true)
      case (Topped.Actual(false), Topped.Actual(false)) => Topped.Actual(false)
      case _ => Topped.Top
    ConstantRelation(rv.cols, newRows, newEmpty)

  override def apply(v1: ConstantRelation, v2: ConstantRelation): MaybeChanged[ConstantRelation] =
    MaybeChanged(join(v1, v2), v1)