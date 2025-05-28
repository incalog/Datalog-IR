package inca.ir.analysis.base.values

import inca.ir.analysis.RelationOps
import inca.ir.analysis.base.effect.{BaseIRException, EmptyTable}
import sturdy.data.WithJoin
import sturdy.effect.except.Except
import sturdy.values.Topped.Top
import sturdy.values.booleans.BooleanOps
import sturdy.values.ordering.EqOps
import sturdy.values.{Join, MaybeChanged, Topped, Widen}

import scala.collection


enum FiniteAbstractRelation:
  case Empty(cs: Seq[String])
  case NonEmpty(cs: Seq[String], rs: Seq[Value], emp: Topped[Boolean], fin: Topped[Boolean])

  def cols: Seq[String] = this match
    case FiniteAbstractRelation.Empty(cols) => cols
    case FiniteAbstractRelation.NonEmpty(cols, rows, empty, finite) => cols

  def rows: Seq[Value] = this match
    case FiniteAbstractRelation.Empty(cs) => throw new IllegalArgumentException()
    case FiniteAbstractRelation.NonEmpty(cs, rows, emp, finite) => rows

  def empty: Topped[Boolean] = this match
    case FiniteAbstractRelation.Empty(cols) => Topped.Actual(true)
    case FiniteAbstractRelation.NonEmpty(cols, rows, empty, finite) => empty

  def finite: Topped[Boolean] = this match
    case FiniteAbstractRelation.Empty(cols) => Topped.Actual(true)
    case FiniteAbstractRelation.NonEmpty(cols, rows, empty, finite) => finite

  override def toString: String = this match
    case Empty(cols) => s"[${cols.mkString(", ")}, true]"
    case NonEmpty(cols, rows, empty, finite) =>
      finite match
        case Topped.Actual(true) =>
          s"[${cols.zip(rows).toMap.mkString(", ")}, $empty]"
        case _ =>
          s"[${cols.zip(rows).toMap.mkString(", ")}, $empty, ∞]"

  def withColumns(newCols: Seq[String]): FiniteAbstractRelation = this match
    case FiniteAbstractRelation.Empty(cols) => Empty(newCols)
    case FiniteAbstractRelation.NonEmpty(cols, rows, empty, finite) => NonEmpty(newCols, rows, empty, finite)

  def withRows(newCols: Seq[String], newRows: Seq[Value] => Seq[Value]): FiniteAbstractRelation = this match
    case FiniteAbstractRelation.Empty(cols) => Empty(newCols)
    case FiniteAbstractRelation.NonEmpty(cols, rows, empty, finite) => FiniteAbstractRelation.NonEmpty(newCols, newRows(rows), empty, finite)


object FiniteAbstractRelation:
  def apply(cols: Seq[String], rows: Seq[Value], empty: Topped[Boolean], finite: Topped[Boolean]): FiniteAbstractRelation =
    //assert(cols.toSet.size == cols.size) // unique columns
    //assert(cols.size == rows.size) // column size matches row size
    FiniteAbstractRelation.NonEmpty(cols, rows, empty, finite)

  def empty(cols: Seq[String]): FiniteAbstractRelation =
    //assert(cols.toSet.size == cols.size) // unique columns
    FiniteAbstractRelation.Empty(cols)

class FiniteAbstractRelationOps[ExcV](using except: Except[BaseIRException, ExcV, WithJoin])
                               (using joinV: Join[Value],
                                meetV: Meet[Value],
                                boolOps: BooleanOps[Topped[Boolean]],
                                eqOps: EqOps[Value, Topped[Boolean]])
  extends RelationOps[Value, Topped[Boolean], FiniteAbstractRelation]:

  def isFinite(rv: FiniteAbstractRelation): Topped[Boolean] = rv.finite

  override def isEmpty(rv: FiniteAbstractRelation): Topped[Boolean] = rv.empty

  override def hasColumn(rv: FiniteAbstractRelation, column: String): Boolean = rv.cols.contains(column)

  override def columns(rv: FiniteAbstractRelation): Seq[String] = rv.cols

  override def make(cols: Seq[String], vals: Seq[Row]): FiniteAbstractRelation =
    if (vals.isEmpty)
      FiniteAbstractRelation.empty(cols)
    else if (vals.size == 1)
      val isFinite = vals.head.forall(_.isFinite)
      if (!isFinite) throw IllegalStateException()
      FiniteAbstractRelation(cols, vals.head, Topped.Actual(vals.isEmpty), Topped.Actual(isFinite))
    else
      // It is not obvious if the implicit behaviour should be a meet or a join. Therefore, we throw an exception.
      throw IllegalStateException("Can not initialize constant relation with more than one row.")
      //val joinedVals = vals.tail.foldLeft[Row](vals.head)((v1, v2) => v1.zip(v2).map((t1, t2) => meetV(t1, t2).get))
      //FiniteAbstractRelation(cols, joinedVals, Topped.Actual(vals.isEmpty))

  override def rename(rv: FiniteAbstractRelation, subst: Map[String, String]): FiniteAbstractRelation =
    val newColumns = rv.cols.map(c => subst.getOrElse(c, c))
    rv.withColumns(newColumns)

  override def project(rv: FiniteAbstractRelation, newColumns: Seq[String]): FiniteAbstractRelation =
    val colsIndex = newColumns.map(rv.cols.indexOf)
    rv.withRows(newColumns, rows => colsIndex.map(rows))

  override def extract(rv: FiniteAbstractRelation, columnNames: Seq[String]): Seq[Row] = rv match
    case FiniteAbstractRelation.Empty(cs) => Seq(Seq())
    case FiniteAbstractRelation.NonEmpty(cs, rs, emp, finite) =>
      val colIndices = columnNames.map(cs.indexOf)
      Seq(colIndices.map(rs))

  override def projectAndRenameWithMultipleAliases(rv: FiniteAbstractRelation, subst: Map[String, Seq[String]]): FiniteAbstractRelation =
    val newCols = subst.values.flatten.toSeq
    rv match
      case FiniteAbstractRelation.Empty(_) => FiniteAbstractRelation.empty(newCols)
      case FiniteAbstractRelation.NonEmpty(_, _, emp, finite) =>
        val newRows = subst.flatMap { case (col, newCols) =>
          val colIndex = rv.cols.indexOf(col)
          val v = rv.rows(colIndex)
          (1 to newCols.size).map(_ => v)
        }.toSeq
        FiniteAbstractRelation(newCols, newRows, emp, finite)

  override def map(rv: FiniteAbstractRelation, columnName: String)(f: Seq[Value] => Value): FiniteAbstractRelation = rv match
    case FiniteAbstractRelation.Empty(cols) =>
      FiniteAbstractRelation.Empty(cols :+ columnName)
    case FiniteAbstractRelation.NonEmpty(cols, rows, empty, finite) =>
      val newV = f(rows)
      val newFinite = boolOps.and(Topped.Actual(newV.isFinite), finite)
      FiniteAbstractRelation.NonEmpty(cols :+ columnName, rows :+ newV, empty, newFinite)

  override def groupBy(rv: FiniteAbstractRelation, accumulatorCols: Seq[String], groupByCols: Seq[String])
                      (newCols: Seq[String], f: (groupByValues: Row, accValues: Seq[Row]) => Row): FiniteAbstractRelation =
    rv match
      case FiniteAbstractRelation.Empty(cols) => FiniteAbstractRelation.empty(newCols)
      case FiniteAbstractRelation.NonEmpty(cols, rows, empty, finite) =>
        val groupyByIndices = groupByCols.map(cols.indexOf) 
        val groupByValues = groupyByIndices.map(rows.apply)
        val accIndices = accumulatorCols.map(cols.indexOf)
        val accValues = accIndices.map(rows.apply)
        val newRows = f(groupByValues, Seq(accValues))
        if (newRows.size != newCols.size)
          throw IllegalStateException("Number of new columns must match arity of new rows.")
        val isFinite = newRows.forall(_.isFinite)
        val newFinite = boolOps.and(Topped.Actual(isFinite), finite)
        FiniteAbstractRelation(newCols, newRows, empty, newFinite)
  
  override def flatMap(rv: FiniteAbstractRelation)(f: Seq[Value] => FiniteAbstractRelation): FiniteAbstractRelation = rv match
    case FiniteAbstractRelation.Empty(cols) => naturalJoin(rv, f(Seq()))
    case FiniteAbstractRelation.NonEmpty(cols, rows, empty, finite) =>
      // TODO: This could also possible produce a top value, which might indicate an infinite relation
      naturalJoin(rv, f(rows))

  override def filter(rv: FiniteAbstractRelation)(f: Seq[Value] => Topped[Boolean]): FiniteAbstractRelation = rv match
    case FiniteAbstractRelation.Empty(cols) => rv
    case rv@FiniteAbstractRelation.NonEmpty(cols, rows, empty, finite) => f(rv.rows) match
      case Topped.Top => rv.copy(emp = Topped.Top)
      case Topped.Actual(true) => rv // unchanged
      case Topped.Actual(false) => FiniteAbstractRelation.Empty(cols) // definitely empty

  override def filter(rv: FiniteAbstractRelation)(f: Seq[Value] => Topped[Boolean])(refine: Seq[Value] => Seq[Value]): FiniteAbstractRelation =
    rv match
      case FiniteAbstractRelation.Empty(cols) => rv
      case rv@FiniteAbstractRelation.NonEmpty(cols, rows, empty, finite) => f(rv.rows) match
        case Topped.Top => FiniteAbstractRelation(cols, refine(rows), Topped.Top, finite)
        case Topped.Actual(true) => FiniteAbstractRelation(cols, refine(rows), empty, finite)
        case Topped.Actual(false) => FiniteAbstractRelation.Empty(cols) // definitely empty

  def filterEq(rv: FiniteAbstractRelation, col: String, col2: String): FiniteAbstractRelation =
    val lix = columnIndex(rv, col)
    val rix = columnIndex(rv, col2)
    val filtered = filter(rv)(row => eqOps.equ(row(lix), row(rix)))
    filtered match
      case _: FiniteAbstractRelation.Empty => filtered
      case FiniteAbstractRelation.NonEmpty(cols, rows, empty, finite) =>
        val meet = meetV(rows(lix), rows(rix)).get
        val newRows = rows.updated(lix, meet).updated(rix, meet)
        FiniteAbstractRelation(cols, newRows, empty, finite)

  def filterNeq(rv: FiniteAbstractRelation, col: String, col2: String): FiniteAbstractRelation =
    val lix = columnIndex(rv, col)
    val rix = columnIndex(rv, col2)
    filter(rv)(row => eqOps.neq(row(lix), row(rix)))

  override def naturalJoin(rv: FiniteAbstractRelation, other: FiniteAbstractRelation): FiniteAbstractRelation =
    val rvCols = rv.cols.zipWithIndex.toMap
    val otherCols = other.cols.zipWithIndex.toMap

    val newCols = rv.cols ++ other.cols.filterNot(rv.cols.contains)
    (rv, other) match
      case (FiniteAbstractRelation.Empty(_), _) | (_, FiniteAbstractRelation.Empty(_)) => FiniteAbstractRelation.empty(newCols)
      case (rv: FiniteAbstractRelation.NonEmpty, other: FiniteAbstractRelation.NonEmpty) =>
        val (newVals, comp) = (for (c <- newCols) yield {
          (rvCols.get(c), otherCols.get(c)) match
            case (Some(rvIx), None) => (rv.rows(rvIx), Topped.Actual(true))
            case (None, Some(otherIx)) => (other.rows(otherIx), Topped.Actual(true))
            case (Some(rvIx), Some(otherIx)) =>
              // We might decide if the join succeeds, if we can compare all values
              val compare = eqOps.equ(rv.rows(rvIx), other.rows(otherIx))
              val v = meetV(rv.rows(rvIx), other.rows(otherIx)).get
              (v, compare)
            case (None, None) => throw new IllegalStateException()
        }).unzip

        val newEmpty = (rv.empty, other.empty) match
          case (Topped.Actual(true), _) | (_, Topped.Actual(true)) =>
            throw IllegalStateException("Comparison should already be handled!")
          case (Topped.Top, _) | (_, Topped.Top) => Topped.Top // we don't know
          case _ if comp.forall(t => t.isActual && t.get) => Topped.Actual(false) // if all comparison succeeded
          case _ if comp.exists(t => t.isActual && !t.get) => Topped.Actual(true) // at least one comparison failed
          case _ => Topped.Top

        val newFinite = (rv.finite, other.finite) match
          case (Topped.Actual(true), Topped.Actual(true)) => Topped.Actual(true)
          case _ => Topped.Top

        newEmpty match
          case Topped.Actual(true) => FiniteAbstractRelation.empty(newCols)
          case _ => FiniteAbstractRelation(newCols, newVals, newEmpty, newFinite)

  override def antiJoin(rv: FiniteAbstractRelation, other: FiniteAbstractRelation): FiniteAbstractRelation =
    (rv, other) match
      case (FiniteAbstractRelation.Empty(_), _) | (_, FiniteAbstractRelation.Empty(_)) => rv
      case (rv: FiniteAbstractRelation.NonEmpty, other: FiniteAbstractRelation.NonEmpty) =>
        val newFinite = rv.finite match
          case Topped.Actual(true) => Topped.Actual(true)
          case _ => Topped.Top
        val sharedCols = rv.cols.intersect(other.cols)
        if (sharedCols.isEmpty)
          throw IllegalArgumentException(s"Not possible to anti join with disjunct columns: ${rv.cols} <-> ${other.cols}")
        (rv.empty, other.empty) match
          case (Topped.Actual(true), Topped.Actual(true)) => FiniteAbstractRelation.empty(rv.cols)
          case (Topped.Actual(false), Topped.Actual(true)) =>
            FiniteAbstractRelation(rv.cols, rv.rows, Topped.Actual(false), newFinite)
          case (Topped.Actual(false), Topped.Actual(false)) =>
            val sameColsIndices = sharedCols.map(rv.cols.indexOf)
            val sameOtherColsIndices = sharedCols.map(other.cols.indexOf)
            val comparison = sameColsIndices.zip(sameOtherColsIndices).map { (rvIx, oIx) =>
              eqOps.equ(rv.rows(rvIx), other.rows(oIx))
            }
            val allComparisonSucceeded = comparison.forall(t => t.isActual && t.get)
            val atLeastOneComparisonFailed = comparison.exists(t => t.isActual && !t.get)
            if (allComparisonSucceeded)
              FiniteAbstractRelation.empty(rv.cols)
            else if (atLeastOneComparisonFailed)
              FiniteAbstractRelation(rv.cols, rv.rows, Topped.Actual(false), newFinite)
            else
              FiniteAbstractRelation(rv.cols, rv.rows, Topped.Top, newFinite)
          case _ =>
            FiniteAbstractRelation(rv.cols, rv.rows, Topped.Top, newFinite)


given FiniteJoinRV(using joinV: Join[Value], boolOps: BooleanOps[Topped[Boolean]], eqOps: EqOps[Value, Topped[Boolean]]): Join[FiniteAbstractRelation] with {
  def join(rv: FiniteAbstractRelation, other: FiniteAbstractRelation): FiniteAbstractRelation =
    if (rv.cols.toSet != other.cols.toSet)
      throw new IllegalArgumentException(s"Schemas must match for join: $rv ++ $other")

    (rv, other) match
      case (FiniteAbstractRelation.Empty(_), _) => other
      case (_, FiniteAbstractRelation.Empty(_)) => rv
      case (rv: FiniteAbstractRelation.NonEmpty, other: FiniteAbstractRelation.NonEmpty) =>
        val others2Rows = rv.cols.map(other.cols.indexOf)
        assert(others2Rows.map(other.cols.apply) == rv.cols)
        // If any of the two relations is definitely non-empty, then the result is also non-empty
        val newEmpty = boolOps.or(rv.empty, other.empty)
        val newRows = rv.rows.zip(others2Rows.map(other.rows.apply)).map { (v1, v2) => joinV(v1, v2).get }
        val newFinite = boolOps.and(rv.finite, other.finite)
        FiniteAbstractRelation(rv.cols, newRows, newEmpty, newFinite)

  override def apply(v1: FiniteAbstractRelation, v2: FiniteAbstractRelation): MaybeChanged[FiniteAbstractRelation] =
    MaybeChanged(join(v1, v2), v1)
}

given FiniteWidenRV(using widenV: Widen[Value], boolOps: BooleanOps[Topped[Boolean]], eqOps: EqOps[Value, Topped[Boolean]]): Widen[FiniteAbstractRelation] with {
  def widen(rv: FiniteAbstractRelation, other: FiniteAbstractRelation): FiniteAbstractRelation =
    if (rv.cols.toSet != other.cols.toSet)
      throw new IllegalArgumentException(s"Schemas must match for join: $rv ++ $other")

    (rv, other) match
      case (FiniteAbstractRelation.Empty(_), _) => other
      case (_, FiniteAbstractRelation.Empty(_)) => rv
      case (rv: FiniteAbstractRelation.NonEmpty, other: FiniteAbstractRelation.NonEmpty) =>
        val others2Rows = rv.cols.map(other.cols.indexOf)
        assert(others2Rows.map(other.cols.apply) == rv.cols)
        // If any of the two relations is definitely non-empty, then the result is also non-empty
        val newEmpty = boolOps.or(rv.empty, other.empty)
        val newRows = rv.rows.zip(others2Rows.map(other.rows.apply)).map { (v1, v2) => widenV(v1, v2).get }
        val newFinite = boolOps.and(rv.finite, other.finite)
        FiniteAbstractRelation(rv.cols, newRows, newEmpty, newFinite)

  override def apply(v1: FiniteAbstractRelation, v2: FiniteAbstractRelation): MaybeChanged[FiniteAbstractRelation] =
    MaybeChanged(widen(v1, v2), v1)
}