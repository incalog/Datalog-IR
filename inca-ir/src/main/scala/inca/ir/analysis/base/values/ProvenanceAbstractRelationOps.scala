package inca.ir.analysis.base.values

import inca.ir.analysis.RelationOps
import inca.ir.analysis.base.effect.BaseIRException
import sturdy.data.WithJoin
import sturdy.effect.except.Except
import sturdy.values.ordering.EqOps
import sturdy.values.{Join, MaybeChanged, Topped, Widen}

case class ProvenanceV(origins: Set[String]) extends Value:
  override def isConstant: Boolean = false

  override def toString: String =
    origins.toSeq.sorted match
      case Seq() => "unknown"
      case Seq(origin) => origin
      case many => many.mkString("{", ", ", "}")

object ProvenanceV:
  val empty: ProvenanceV = ProvenanceV(Set.empty)
  def apply(origin: String): ProvenanceV = ProvenanceV(Set(origin))


case class ProvenanceAbstractRelation(cols: Seq[String], rows: Seq[Value]):
  def empty: Topped[Boolean] = Topped.Actual(false)

  override def toString: String =
    s"[${cols.zip(rows).toMap.mkString(", ")}]"

  def withColumns(newCols: Seq[String]): ProvenanceAbstractRelation =
    ProvenanceAbstractRelation(newCols, rows)

  def withRows(newCols: Seq[String], newRows: Seq[Value] => Seq[Value]): ProvenanceAbstractRelation =
    ProvenanceAbstractRelation(newCols, newRows(rows))


class ProvenanceAbstractRelationOps[ExcV](using except: Except[BaseIRException, ExcV, WithJoin])
                                         (using joinV: Join[Value],
                                          eqOps: EqOps[Value, Topped[Boolean]])
  extends RelationOps[Value, Topped[Boolean], ProvenanceAbstractRelation]:

  override def isEmpty(rv: ProvenanceAbstractRelation): Topped[Boolean] = rv.empty

  override def hasColumn(rv: ProvenanceAbstractRelation, column: String): Boolean = rv.cols.contains(column)

  override def columns(rv: ProvenanceAbstractRelation): Seq[String] = rv.cols

  override def make(cols: Seq[String], vals: Seq[Row]): ProvenanceAbstractRelation =
    if (vals.isEmpty)
      ProvenanceAbstractRelation(cols, cols.map(_ => ProvenanceV.empty))
    else if (vals.size == 1)
      ProvenanceAbstractRelation(cols, vals.head)
    else
      throw IllegalStateException("Cannot initialize provenance relation with more than one abstract row.")

  override def rename(rv: ProvenanceAbstractRelation, subst: Map[String, String]): ProvenanceAbstractRelation =
    rv.withColumns(rv.cols.map(c => subst.getOrElse(c, c)))

  override def project(rv: ProvenanceAbstractRelation, newColumns: Seq[String]): ProvenanceAbstractRelation =
    val colsIndex = newColumns.map(rv.cols.indexOf)
    rv.withRows(newColumns, rows => colsIndex.map(rows))

  override def extract(rv: ProvenanceAbstractRelation, columnNames: Seq[String]): Seq[Row] =
    val colIndices = columnNames.map(rv.cols.indexOf)
    Seq(colIndices.map(rv.rows))

  override def projectAndRenameWithMultipleAliases(rv: ProvenanceAbstractRelation, subst: Map[String, Seq[String]]): ProvenanceAbstractRelation =
    val newCols = subst.values.flatten.toSeq
    val newRows = subst.flatMap { case (col, aliases) =>
      val colIndex = rv.cols.indexOf(col)
      val v = rv.rows(colIndex)
      aliases.map(_ => v)
    }.toSeq
    ProvenanceAbstractRelation(newCols, newRows)

  override def map(rv: ProvenanceAbstractRelation, columnName: String)(f: Seq[Value] => Value): ProvenanceAbstractRelation =
    ProvenanceAbstractRelation(rv.cols :+ columnName, rv.rows :+ f(rv.rows))

  override def groupBy(rv: ProvenanceAbstractRelation, accumulatorCols: Seq[String], groupByCols: Seq[String])
                      (newCols: Seq[String], f: (groupByValues: Row, accValues: Seq[Row]) => Row): ProvenanceAbstractRelation =
    val groupByIndices = groupByCols.map(rv.cols.indexOf)
    val groupByValues = groupByIndices.map(rv.rows.apply)
    val accIndices = accumulatorCols.map(rv.cols.indexOf)
    val accValues = accIndices.map(rv.rows.apply)
    val newRows = f(groupByValues, Seq(accValues))
    if (newRows.size != newCols.size)
      throw IllegalStateException("Number of new columns must match arity of new rows.")
    ProvenanceAbstractRelation(newCols, newRows)

  override def flatMap(rv: ProvenanceAbstractRelation)(f: Seq[Value] => ProvenanceAbstractRelation): ProvenanceAbstractRelation =
    naturalJoin(rv, f(rv.rows))

  override def filter(rv: ProvenanceAbstractRelation)(f: Seq[Value] => Topped[Boolean]): ProvenanceAbstractRelation =
    rv

  override def filter(rv: ProvenanceAbstractRelation)(f: Seq[Value] => Topped[Boolean])(refine: Seq[Value] => Seq[Value]): ProvenanceAbstractRelation =
    rv

  override def filterEq(rv: ProvenanceAbstractRelation, col: String, col2: String): ProvenanceAbstractRelation =
    val lix = columnIndex(rv, col)
    val rix = columnIndex(rv, col2)
    val join = joinV(rv.rows(lix), rv.rows(rix)).get
    val newRows = rv.rows.updated(lix, join).updated(rix, join)
    ProvenanceAbstractRelation(rv.cols, newRows)

  override def filterNeq(rv: ProvenanceAbstractRelation, col: String, col2: String): ProvenanceAbstractRelation =
    rv

  override def naturalJoin(rv: ProvenanceAbstractRelation, other: ProvenanceAbstractRelation): ProvenanceAbstractRelation =
    val rvCols = rv.cols.zipWithIndex.toMap
    val otherCols = other.cols.zipWithIndex.toMap
    val newCols = rv.cols ++ other.cols.filterNot(rv.cols.contains)

    val newVals = newCols.map { c =>
      (rvCols.get(c), otherCols.get(c)) match
        case (Some(rvIx), None) => rv.rows(rvIx)
        case (None, Some(otherIx)) => other.rows(otherIx)
        case (Some(rvIx), Some(otherIx)) => joinV(rv.rows(rvIx), other.rows(otherIx)).get
        case (None, None) => throw new IllegalStateException()
    }

    ProvenanceAbstractRelation(newCols, newVals)

  override def antiJoin(rv: ProvenanceAbstractRelation, other: ProvenanceAbstractRelation): ProvenanceAbstractRelation =
    rv


given ProvenanceJoinRV(using joinV: Join[Value]): Join[ProvenanceAbstractRelation] with {
  def join(rv: ProvenanceAbstractRelation, other: ProvenanceAbstractRelation): ProvenanceAbstractRelation =
    if (rv.cols.toSet != other.cols.toSet)
      throw new IllegalArgumentException(s"Schemas must match for join: $rv ++ $other")

    val others2Rows = rv.cols.map(other.cols.indexOf)
    assert(others2Rows.map(other.cols.apply) == rv.cols)
    val newRows = rv.rows.zip(others2Rows.map(other.rows.apply)).map { (v1, v2) => joinV(v1, v2).get }
    ProvenanceAbstractRelation(rv.cols, newRows)

  override def apply(v1: ProvenanceAbstractRelation, v2: ProvenanceAbstractRelation): MaybeChanged[ProvenanceAbstractRelation] =
    MaybeChanged(join(v1, v2), v1)
}

given ProvenanceWidenRV(using widenV: Widen[Value]): Widen[ProvenanceAbstractRelation] with {
  def widen(rv: ProvenanceAbstractRelation, other: ProvenanceAbstractRelation): ProvenanceAbstractRelation =
    if (rv.cols.toSet != other.cols.toSet)
      throw new IllegalArgumentException(s"Schemas must match for widen: $rv ++ $other")

    val others2Rows = rv.cols.map(other.cols.indexOf)
    assert(others2Rows.map(other.cols.apply) == rv.cols)
    val newRows = rv.rows.zip(others2Rows.map(other.rows.apply)).map { (v1, v2) => widenV(v1, v2).get }
    ProvenanceAbstractRelation(rv.cols, newRows)

  override def apply(v1: ProvenanceAbstractRelation, v2: ProvenanceAbstractRelation): MaybeChanged[ProvenanceAbstractRelation] =
    MaybeChanged(widen(v1, v2), v1)
}
