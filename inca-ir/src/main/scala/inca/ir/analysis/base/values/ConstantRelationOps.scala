package inca.ir.analysis.base.values

import inca.ir.analysis.RelationOps
import inca.ir.analysis.base.effect.{BaseIRException, EmptyTable}
import sturdy.data.WithJoin
import sturdy.effect.except.Except
import sturdy.values.booleans.BooleanOps
import sturdy.values.ordering.EqOps
import sturdy.values.{Join, MaybeChanged, Topped}

import scala.collection

// We can not decide if a table is empty or not.
// Consider we have a body that contains a comparison such as: Top == ConstantIntV(4)
// This could succeed, but it could also fail.
enum ConstantRelation:
  case Empty(cs: Seq[String])
  case NonEmpty(cs: Seq[String], rs:Seq[Value], emp: Topped[Boolean])

  def cols: Seq[String] = this match
    case ConstantRelation.Empty(cols) => cols
    case ConstantRelation.NonEmpty(cols, rows, empty) => cols

  def rows: Seq[Value] = this match
    case ConstantRelation.Empty(cs) => throw new IllegalArgumentException()
    case ConstantRelation.NonEmpty(cs, rows, emp) => rows

  def empty: Topped[Boolean] = this match
    case ConstantRelation.Empty(cols) => Topped.Actual(true)
    case ConstantRelation.NonEmpty(cols, rows, empty) => empty

  override def toString: String = this match
    case Empty(cols) => s"[${cols.mkString(", ")}, empty]"
    case NonEmpty(cols, rows, empty) => s"[${cols.zip(rows).toMap.mkString(", ")}, $empty]"

  def withColumns(newCols: Seq[String]): ConstantRelation = this match
    case ConstantRelation.Empty(cols) => Empty(newCols)
    case ConstantRelation.NonEmpty(cols, rows, empty) => NonEmpty(newCols, rows, empty)

  def withRows(newCols: Seq[String], newRows: Seq[Value] => Seq[Value]): ConstantRelation = this match
    case ConstantRelation.Empty(cols) => Empty(newCols)
    case ConstantRelation.NonEmpty(cols, rows, empty) => ConstantRelation.NonEmpty(newCols, newRows(rows), empty)

object ConstantRelation {
  def apply(cols: Seq[String], rows: Seq[Value], empty: Topped[Boolean]): ConstantRelation =
    ConstantRelation.NonEmpty(cols, rows, empty)
}

class ConstantRelationOps[ExcV](using except: Except[BaseIRException, ExcV, WithJoin])
                               (using joinV: Join[Value], boolOps: BooleanOps[Topped[Boolean]], eqOps: EqOps[Value, Topped[Boolean]])
  extends RelationOps[Value, Topped[Boolean], ConstantRelation]:

  override def isEmpty(rv: ConstantRelation): Topped[Boolean] = rv.empty

  override def hasColumn(rv: ConstantRelation, column: String): Boolean = rv.cols.contains(column)

  override def columns(rv: ConstantRelation): Seq[String] = rv.cols

  override def make(cols: Seq[String], vals: Seq[Row]): ConstantRelation =
    if (vals.isEmpty)
      ConstantRelation.Empty(cols)
    else {
      val joinedVals = vals.tail.foldLeft[Row](vals.head)((v1, v2) => v1.zip(v2).map((t1, t2) => joinV(t1, t2).get))
      ConstantRelation(cols, joinedVals, Topped.Actual(vals.isEmpty))
    }

  override def rename(rv: ConstantRelation, subst: Map[String, String]): ConstantRelation =
    val newColumns = rv.cols.map(c => subst.getOrElse(c, c))
    rv.withColumns(newColumns)

  override def project(rv: ConstantRelation, newColumns: Seq[String]): ConstantRelation =
    val colsIndex = newColumns.map(rv.cols.indexOf)
    rv.withRows(newColumns, rows => colsIndex.map(rows))

  override def map(rv: ConstantRelation, columnName: String)(f: Seq[Value] => Value): ConstantRelation =
    rv.withRows(rv.cols :+ columnName, rows => rows :+ f(rows))

  override def fold(rv: ConstantRelation, initial: Row)(f: (Row, Row) => Row): ConstantRelation =
    rv match
      case ConstantRelation.Empty(cols) =>
        ConstantRelation(rv.cols, initial, empty = Topped.Actual(false))
      case ConstantRelation.NonEmpty(cols, rows, empty) =>
        ConstantRelation(cols, f(initial, rows), empty = Topped.Actual(false))
  
  override def flatMap(rv: ConstantRelation)(f: Seq[Value] => ConstantRelation): ConstantRelation =
    rv match
      case ConstantRelation.Empty(cols) => naturalJoin(rv, f(Seq()))
      case ConstantRelation.NonEmpty(cols, rows, empty) => naturalJoin(rv, f(rows))

  override def filter(rv: ConstantRelation)(f: Seq[Value] => Topped[Boolean]): ConstantRelation =
    rv match
      case ConstantRelation.Empty(cols) => rv
      case rv@ConstantRelation.NonEmpty(cols, rows, empty) => f(rv.rows) match
        case Topped.Top => rv.copy(emp = Topped.Top)
        case Topped.Actual(true) => rv // unchanged
        case Topped.Actual(false) => rv.copy(emp = Topped.Actual(true)) // definitely empty

  override def naturalJoin(rv: ConstantRelation, other: ConstantRelation): ConstantRelation =
    val rvCols = rv.cols.zipWithIndex.toMap
    val otherCols = other.cols.zipWithIndex.toMap

    val newCols = rv.cols ++ other.cols.filterNot(rv.cols.contains)
    var newEmpty = (rv.empty, other.empty) match
      case (Topped.Actual(true), _) | (_, Topped.Actual(true)) => Topped.Actual(true) // definitely empty
      case (Topped.Top, _) | (_, Topped.Top) => Topped.Top // we don't know
      case _ => Topped.Actual(false) // we need to refine the result

    (rv, other) match
      case (ConstantRelation.Empty(_), _) | (_, ConstantRelation.Empty(_)) => ConstantRelation.Empty(newCols)
      case (rv: ConstantRelation.NonEmpty, other: ConstantRelation.NonEmpty) =>
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
    (rv, other) match
      case (ConstantRelation.Empty(_), _) | (_, ConstantRelation.Empty(_)) => rv
      case (rv: ConstantRelation.NonEmpty, other: ConstantRelation.NonEmpty) =>
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

    (rv, other) match
      case (ConstantRelation.Empty(_), _) => other
      case (_, ConstantRelation.Empty(_)) => rv
      case (rv: ConstantRelation.NonEmpty, other: ConstantRelation.NonEmpty) =>
        val others2Rows = other.cols.map(rv.cols.indexOf)
        val newEmpty = boolOps.and(rv.empty, other.empty)
        val newRows = rv.rows.zip(others2Rows.map(other.rows.apply)).map { (v1, v2) => joinV(v1, v2).get }
        ConstantRelation(rv.cols, newRows, newEmpty)

  override def apply(v1: ConstantRelation, v2: ConstantRelation): MaybeChanged[ConstantRelation] =
    MaybeChanged(join(v1, v2), v1)
}