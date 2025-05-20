package inca.ir.analysis.base.values

import inca.ir.analysis.RelationOps
import inca.ir.analysis.base.effect.{BaseIRException, EmptyTable}
import inca.ir.analysis.base.values.AbstractRelation.Empty
import sturdy.data.WithJoin
import sturdy.effect.except.Except
import sturdy.values.Topped.Top
import sturdy.values.booleans.BooleanOps
import sturdy.values.ordering.EqOps
import sturdy.values.{Join, MaybeChanged, Topped, Widen}

import scala.collection

// We can not decide if a relation is empty or not.
// E.g. Consider a type analysis:
//  TInt == TInt
// This could be comparing any pair of integers.
// Or consider with a constant analysis that analysis an atom:
//  Top == ConstantIntV(4)
// This could succeed, but it could also fail.

enum AbstractRelation:
  case Empty(cs: Seq[String])
  case NonEmpty(cs: Seq[String], rs: Seq[Value], emp: Topped[Boolean])

  def cols: Seq[String] = this match
    case AbstractRelation.Empty(cols) => cols
    case AbstractRelation.NonEmpty(cols, rows, empty) => cols

  def rows: Seq[Value] = this match
    case AbstractRelation.Empty(cs) => throw new IllegalArgumentException()
    case AbstractRelation.NonEmpty(cs, rows, emp) => rows

  def empty: Topped[Boolean] = this match
    case AbstractRelation.Empty(cols) => Topped.Actual(true)
    case AbstractRelation.NonEmpty(cols, rows, empty) => empty

  override def toString: String = this match
    case Empty(cols) => s"[${cols.mkString(", ")}, true]"
    case NonEmpty(cols, rows, empty) => s"[${cols.zip(rows).toMap.mkString(", ")}, $empty]"

  def withColumns(newCols: Seq[String]): AbstractRelation = this match
    case AbstractRelation.Empty(cols) => Empty(newCols)
    case AbstractRelation.NonEmpty(cols, rows, empty) => NonEmpty(newCols, rows, empty)

  def withRows(newCols: Seq[String], newRows: Seq[Value] => Seq[Value]): AbstractRelation = this match
    case AbstractRelation.Empty(cols) => Empty(newCols)
    case AbstractRelation.NonEmpty(cols, rows, empty) => AbstractRelation.NonEmpty(newCols, newRows(rows), empty)


object AbstractRelation:
  def apply(cols: Seq[String], rows: Seq[Value], empty: Topped[Boolean]): AbstractRelation =
    //assert(cols.toSet.size == cols.size) // unique columns
    //assert(cols.size == rows.size) // column size matches row size
    AbstractRelation.NonEmpty(cols, rows, empty)

  def empty(cols: Seq[String]): AbstractRelation =
    //assert(cols.toSet.size == cols.size) // unique columns
    AbstractRelation.Empty(cols)

class AbstractRelationOps[ExcV](using except: Except[BaseIRException, ExcV, WithJoin])
                               (using joinV: Join[Value],
                                meetV: Meet[Value],
                                boolOps: BooleanOps[Topped[Boolean]],
                                eqOps: EqOps[Value, Topped[Boolean]])
  extends RelationOps[Value, Topped[Boolean], AbstractRelation]:

  override def isEmpty(rv: AbstractRelation): Topped[Boolean] = rv.empty

  override def hasColumn(rv: AbstractRelation, column: String): Boolean = rv.cols.contains(column)

  override def columns(rv: AbstractRelation): Seq[String] = rv.cols

  override def make(cols: Seq[String], vals: Seq[Row]): AbstractRelation =
    if (vals.isEmpty)
      AbstractRelation.empty(cols)
    else if (vals.size == 1)
      AbstractRelation(cols, vals.head, Topped.Actual(vals.isEmpty))
    else
      // It is not obvious if the implicit behaviour should be a meet or a join. Therefore, we throw an exception.
      throw IllegalStateException("Can not initialize constant relation with more than one row.")
      //val joinedVals = vals.tail.foldLeft[Row](vals.head)((v1, v2) => v1.zip(v2).map((t1, t2) => meetV(t1, t2).get))
      //AbstractRelation(cols, joinedVals, Topped.Actual(vals.isEmpty))

  override def rename(rv: AbstractRelation, subst: Map[String, String]): AbstractRelation =
    val newColumns = rv.cols.map(c => subst.getOrElse(c, c))
    rv.withColumns(newColumns)

  override def project(rv: AbstractRelation, newColumns: Seq[String]): AbstractRelation =
    val colsIndex = newColumns.map(rv.cols.indexOf)
    rv.withRows(newColumns, rows => colsIndex.map(rows))

  override def extract(rv: AbstractRelation, columnNames: Seq[String]): Seq[Row] = rv match
    case AbstractRelation.Empty(cs) => Seq(Seq())
    case AbstractRelation.NonEmpty(cs, rs, emp) =>
      val colIndices = columnNames.map(cs.indexOf)
      Seq(colIndices.map(rs))

  override def projectAndRenameWithMultipleAliases(rv: AbstractRelation, subst: Map[String, Seq[String]]): AbstractRelation =
    val newCols = subst.values.flatten.toSeq
    rv match
      case AbstractRelation.Empty(_) => AbstractRelation.empty(newCols)
      case AbstractRelation.NonEmpty(_, _, emp) =>
        val newRows = subst.flatMap { case (col, newCols) =>
          val colIndex = rv.cols.indexOf(col)
          val v = rv.rows(colIndex)
          (1 to newCols.size).map(_ => v)
        }.toSeq
        AbstractRelation(newCols, newRows, emp)
    
  
  override def map(rv: AbstractRelation, columnName: String)(f: Seq[Value] => Value): AbstractRelation =
    rv.withRows(rv.cols :+ columnName, rows => rows :+ f(rows))

  override def groupBy(rv: AbstractRelation, accumulatorCols: Seq[String], groupByCols: Seq[String])
                      (newCols: Seq[String], f: (groupByValues: Row, accValues: Seq[Row]) => Row): AbstractRelation =
    rv match
      case AbstractRelation.Empty(cols) => AbstractRelation.empty(newCols)
      case AbstractRelation.NonEmpty(cols, rows, empty) =>
        val groupyByIndices = groupByCols.map(cols.indexOf) 
        val groupByValues = groupyByIndices.map(rows.apply)
        val accIndices = accumulatorCols.map(cols.indexOf)
        val accValues = accIndices.map(rows.apply)
        val newRows = f(groupByValues, Seq(accValues))
        if (newRows.size != newCols.size)
          throw IllegalStateException("Number of new columns must match arity of new rows.")
        AbstractRelation(newCols, newRows, empty)
  
  override def flatMap(rv: AbstractRelation)(f: Seq[Value] => AbstractRelation): AbstractRelation = rv match
    case AbstractRelation.Empty(cols) => naturalJoin(rv, f(Seq()))
    case AbstractRelation.NonEmpty(cols, rows, empty) => naturalJoin(rv, f(rows))

  override def filter(rv: AbstractRelation)(f: Seq[Value] => Topped[Boolean]): AbstractRelation = rv match
    case AbstractRelation.Empty(cols) => rv
    case rv@AbstractRelation.NonEmpty(cols, rows, empty) => f(rv.rows) match
      case Topped.Top => rv.copy(emp = Topped.Top)
      case Topped.Actual(true) => rv // unchanged
      case Topped.Actual(false) => rv.copy(emp = Topped.Actual(true)) // definitely empty

  override def filter(rv: AbstractRelation)(f: Seq[Value] => Topped[Boolean])(refine: Seq[Value] => Seq[Value]): AbstractRelation =
    rv match
      case AbstractRelation.Empty(cols) => rv
      case rv@AbstractRelation.NonEmpty(cols, rows, empty) => f(rv.rows) match
        case Topped.Top => AbstractRelation(cols, refine(rows), Topped.Top)
        case Topped.Actual(true) => AbstractRelation(cols, refine(rows), empty)
        case Topped.Actual(false) => AbstractRelation(cols, refine(rows), Topped.Actual(true)) // definitely empty

  def filterEq(rv: AbstractRelation, col: String, col2: String): AbstractRelation =
    val lix = columnIndex(rv, col)
    val rix = columnIndex(rv, col2)
    val filtered = filter(rv)(row => eqOps.equ(row(lix), row(rix)))
    filtered match
      case _: AbstractRelation.Empty => filtered
      case AbstractRelation.NonEmpty(cols, rows, empty) =>
        val meet = meetV(rows(lix), rows(rix)).get
        val newRows = rows.updated(lix, meet).updated(rix, meet)
        AbstractRelation(cols, newRows, empty)

  def filterNeq(rv: AbstractRelation, col: String, col2: String): AbstractRelation =
    val lix = columnIndex(rv, col)
    val rix = columnIndex(rv, col2)
    filter(rv)(row => eqOps.neq(row(lix), row(rix)))

  override def naturalJoin(rv: AbstractRelation, other: AbstractRelation): AbstractRelation =
    val rvCols = rv.cols.zipWithIndex.toMap
    val otherCols = other.cols.zipWithIndex.toMap

    val newCols = rv.cols ++ other.cols.filterNot(rv.cols.contains)
    (rv, other) match
      case (AbstractRelation.Empty(_), _) | (_, AbstractRelation.Empty(_)) => AbstractRelation.empty(newCols)
      case (rv: AbstractRelation.NonEmpty, other: AbstractRelation.NonEmpty) =>
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

        newEmpty match
          case Topped.Actual(true) => AbstractRelation.empty(newCols)
          case _ => AbstractRelation(newCols, newVals, newEmpty)

  override def antiJoin(rv: AbstractRelation, other: AbstractRelation): AbstractRelation =
    (rv, other) match
      case (AbstractRelation.Empty(_), _) | (_, AbstractRelation.Empty(_)) => rv
      case (rv: AbstractRelation.NonEmpty, other: AbstractRelation.NonEmpty) =>
        val sharedCols = rv.cols.intersect(other.cols)
        if (sharedCols.isEmpty)
          throw IllegalArgumentException(s"Not possible to anti join with disjunct columns: ${rv.cols} <-> ${other.cols}")
        (rv.empty, other.empty) match
          case (Topped.Actual(true), Topped.Actual(true)) => AbstractRelation.empty(rv.cols)
          case (Topped.Actual(false), Topped.Actual(true)) => AbstractRelation(rv.cols, rv.rows, Topped.Actual(false))
          case (Topped.Actual(false), Topped.Actual(false)) =>
            val sameColsIndices = sharedCols.map(rv.cols.indexOf)
            val sameOtherColsIndices = sharedCols.map(other.cols.indexOf)
            val comparison = sameColsIndices.zip(sameOtherColsIndices).map { (rvIx, oIx) =>
              eqOps.equ(rv.rows(rvIx), other.rows(oIx))
            }
            val allComparisonSucceeded = comparison.forall(t => t.isActual && t.get)
            val atLeastOneComparisonFailed = comparison.exists(t => t.isActual && !t.get)
            if (allComparisonSucceeded)
              AbstractRelation.empty(rv.cols)
            else if (atLeastOneComparisonFailed)
              AbstractRelation(rv.cols, rv.rows, Topped.Actual(false))
            else
              AbstractRelation(rv.cols, rv.rows, Topped.Top)
          case _ =>
            AbstractRelation(rv.cols, rv.rows, Topped.Top)


given JoinRV(using joinV: Join[Value], boolOps: BooleanOps[Topped[Boolean]], eqOps: EqOps[Value, Topped[Boolean]]): Join[AbstractRelation] with {
  def join(rv: AbstractRelation, other: AbstractRelation): AbstractRelation =
    if (rv.cols.toSet != other.cols.toSet)
      throw new IllegalArgumentException(s"Schemas must match for join: $rv ++ $other")

    (rv, other) match
      case (AbstractRelation.Empty(_), _) => other
      case (_, AbstractRelation.Empty(_)) => rv
      case (rv: AbstractRelation.NonEmpty, other: AbstractRelation.NonEmpty) =>
        val others2Rows = rv.cols.map(other.cols.indexOf)
        assert(others2Rows.map(other.cols.apply) == rv.cols)
        // If any of the two relations is definitely non-empty, then the result is also non-empty
        val newEmpty = boolOps.or(rv.empty, other.empty)
        val newRows = rv.rows.zip(others2Rows.map(other.rows.apply)).map { (v1, v2) => joinV(v1, v2).get }
        AbstractRelation(rv.cols, newRows, newEmpty)

  override def apply(v1: AbstractRelation, v2: AbstractRelation): MaybeChanged[AbstractRelation] =
    MaybeChanged(join(v1, v2), v1)
}

given WidenRV(using widenV: Widen[Value], boolOps: BooleanOps[Topped[Boolean]], eqOps: EqOps[Value, Topped[Boolean]]): Widen[AbstractRelation] with {
  def widen(rv: AbstractRelation, other: AbstractRelation): AbstractRelation =
    if (rv.cols.toSet != other.cols.toSet)
      throw new IllegalArgumentException(s"Schemas must match for join: $rv ++ $other")

    (rv, other) match
      case (AbstractRelation.Empty(_), _) => other
      case (_, AbstractRelation.Empty(_)) => rv
      case (rv: AbstractRelation.NonEmpty, other: AbstractRelation.NonEmpty) =>
        val others2Rows = rv.cols.map(other.cols.indexOf)
        assert(others2Rows.map(other.cols.apply) == rv.cols)
        // If any of the two relations is definitely non-empty, then the result is also non-empty
        val newEmpty = boolOps.or(rv.empty, other.empty)
        val newRows = rv.rows.zip(others2Rows.map(other.rows.apply)).map { (v1, v2) => widenV(v1, v2).get }
        AbstractRelation(rv.cols, newRows, newEmpty)

  override def apply(v1: AbstractRelation, v2: AbstractRelation): MaybeChanged[AbstractRelation] =
    MaybeChanged(widen(v1, v2), v1)
}