package inca.ir.analysis.base.values

import inca.ir.Type
import inca.ir.analysis.RelationOps
import inca.ir.analysis.base.effect.{BaseIRException, EmptyTable}
import sturdy.data.CombineEquiSeq
import sturdy.data.MayJoin.WithJoin
import sturdy.effect.except.Except
import sturdy.values
import inca.ir.analysis.base.values.Value
import sturdy.values.MaybeChanged.Unchanged
import sturdy.values.{Topped, *}
import sturdy.values.booleans.BooleanOps
import sturdy.values.ordering.EqOps

// TODO eliminate Bootom value, as done for ConstantRelation

case class AType(ty: Type) extends Value:
  override def isConstant: Boolean = true

def joinTypeValue(v1: Value, v2: Value): Value = (v2, v2) match
  case (Value.Top, _) | (_, Value.Top) => Value.Top
  case (AType(ty1), AType(ty2)) if (ty1 == ty2) => v1
  case _ => Value.Top

enum TypeRelation:
  case Empty(cs: Seq[String])
  case NonEmpty(cs: Seq[String], rs: Seq[Value], emp: Topped[Boolean])

  def cols: Seq[String] = this match
    case Empty(cols) => cols
    case NonEmpty(cols, rows, empty) => cols

  def rows: Seq[Value] = this match
    case Empty(cs) => throw new IllegalArgumentException()
    case NonEmpty(cs, rows, emp) => rows

  def empty: Topped[Boolean] = this match
    case Empty(cols) => Topped.Actual(true)
    case NonEmpty(cols, rows, empty) => empty

  override def toString: String = this match
    case Empty(cols) => s"[${cols.mkString(", ")}, empty]"
    case NonEmpty(cols, rows, empty) => s"[${cols.zip(rows).toMap.mkString(", ")}, $empty]"

  def withColumns(newCols: Seq[String]): TypeRelation = this match
    case Empty(cols) => Empty(newCols)
    case NonEmpty(cols, rows, empty) => NonEmpty(newCols, rows, empty)

  def withRows(newCols: Seq[String], newRows: Seq[Value] => Seq[Value]): TypeRelation = this match
    case Empty(cols) => Empty(newCols)
    case NonEmpty(cols, rows, empty) => NonEmpty(newCols, newRows(rows), empty)


object TypeRelation {
  def apply(cols: Seq[String], rows: Seq[Value], empty: Topped[Boolean]): TypeRelation =
    NonEmpty(cols, rows, empty)
}

class TypeRelationOps[ExcV](using except: Except[BaseIRException, ExcV, WithJoin])
                               (using joinV: Join[Value], boolOps: BooleanOps[Topped[Boolean]], eqOps: EqOps[Value, Topped[Boolean]])
  extends RelationOps[Value, Topped[Boolean], TypeRelation]:

  private def meet(v1: Value, v2: Value): Value = (v1, v2) match
    case (Value.Top, Value.Top) => Value.Top
    case (Value.Top, other: AType) => other
    case (other: AType, Value.Top) => other
    case (AType(ty1), AType(ty2)) if ty1 == ty2 => v1
    case (AType(ty1), AType(ty2)) if ty1 != ty2 => except.throws(EmptyTable)
    case _ => throw IllegalStateException(s"Can not compute meet between $v1 and $v2")

  override def isEmpty(rv: TypeRelation): Topped[Boolean] = rv.empty

  override def hasColumn(rv: TypeRelation, column: String): Boolean = rv.cols.contains(column)

  override def columns(rv: TypeRelation): Seq[String] = rv.cols

  override def make(cols: Seq[String], vals: Seq[Row]): TypeRelation =
    if (vals.isEmpty)
      TypeRelation.Empty(cols)
    else {
      val joinedVals = vals.tail.foldLeft[Row](vals.head)((v1, v2) => v1.zip(v2).map((t1, t2) => joinV(t1, t2).get))
      TypeRelation(cols, joinedVals, Topped.Actual(vals.isEmpty))
    }

  override def rename(rv: TypeRelation, subst: Map[String, String]): TypeRelation =
    val newColumns = rv.cols.map(c => subst.getOrElse(c, c))
    rv.withColumns(newColumns)

  override def project(rv: TypeRelation, newColumns: Seq[String]): TypeRelation =
    val colsIndex = newColumns.map(rv.cols.indexOf)
    rv.withRows(newColumns, rows => colsIndex.map(rows))

  override def map(rv: TypeRelation, columnName: String)(f: Seq[Value] => Value): TypeRelation =
    rv.withRows(rv.cols :+ columnName, rows => rows :+ f(rows))

  override def fold(rv: TypeRelation, initial: Row)(f: (Row, Row) => Row): TypeRelation =
    rv match
      case TypeRelation.Empty(cols) =>
        TypeRelation(rv.cols, initial, empty = Topped.Actual(false))
      case TypeRelation.NonEmpty(cols, rows, empty) =>
        TypeRelation(cols, f(initial, rows), empty = Topped.Actual(false))

  override def flatMap(rv: TypeRelation)(f: Seq[Value] => TypeRelation): TypeRelation =
    rv match
      case TypeRelation.Empty(cols) => naturalJoin(rv, f(Seq()))
      case TypeRelation.NonEmpty(cols, rows, empty) => naturalJoin(rv, f(rows))

  override def filter(rv: TypeRelation)(f: Seq[Value] => Topped[Boolean]): TypeRelation =
    rv match
      case TypeRelation.Empty(cols) => rv
      case rv@TypeRelation.NonEmpty(cols, rows, empty) => f(rv.rows) match
        case Topped.Top => rv.copy(emp = Topped.Top)
        case Topped.Actual(true) => rv // unchanged
        case Topped.Actual(false) => TypeRelation.Empty(cols) //rv.copy(emp = Topped.Actual(true)) // definitely empty

  def filterEq(rv: TypeRelation, col: String, col2: String): TypeRelation =
    val lix = columnIndex(rv, col)
    val rix = columnIndex(rv, col2)
    filter(rv)(row => eqOps.equ(row(lix), row(rix)))

  def filterNeq(rv: TypeRelation, col: String, col2: String): TypeRelation =
    val lix = columnIndex(rv, col)
    val rix = columnIndex(rv, col2)
    filter(rv)(row => eqOps.neq(row(lix), row(rix)))
  
  override def naturalJoin(rv: TypeRelation, other: TypeRelation): TypeRelation =
    val rvCols = rv.cols.zipWithIndex.toMap
    val otherCols = other.cols.zipWithIndex.toMap

    val newCols = rv.cols ++ other.cols.filterNot(rv.cols.contains)

    (rv, other) match
      case (TypeRelation.Empty(_), _) | (_, TypeRelation.Empty(_)) => TypeRelation.Empty(newCols)
      case (rv: TypeRelation.NonEmpty, other: TypeRelation.NonEmpty) =>
        val newTypes = for (c <- newCols) yield {
          (rvCols.get(c), otherCols.get(c)) match
            case (Some(rvIx), None) => rv.rows(rvIx)
            case (None, Some(otherIx)) => other.rows(otherIx)
            case (Some(rvIx), Some(otherIx)) => meet(rv.rows(rvIx), other.rows(otherIx))
            case (None, None) => throw new IllegalStateException()
        }
        val isUnitTable = other.cols.isEmpty && other.emp.isActual && !other.emp.get
        val newEmpty = if (isUnitTable) Topped.Actual(false) else Topped.Top
        TypeRelation(newCols, newTypes, newEmpty)

  override def antiJoin(rv: TypeRelation, other: TypeRelation): TypeRelation =
    val sharedCols = rv.cols.intersect(other.cols)
    if (sharedCols.isEmpty)
      throw IllegalArgumentException(s"Not possible to anti join with disjunct columns: ${rv.cols} <-> ${other.cols}")
    (rv, other) match
      case (TypeRelation.Empty(_), _) | (_, TypeRelation.Empty(_)) => TypeRelation.Empty(rv.cols)
      case (rv: TypeRelation.NonEmpty, other: TypeRelation.NonEmpty) => TypeRelation(rv.cols, rv.rows, Topped.Top)


given JoinTV: Join[Value] with {
  override def apply(v1: Value, v2: Value): MaybeChanged[Value] =
    MaybeChanged(joinTypeValue(v1, v2), v1)
}

given JoinTRV(using boolOps: BooleanOps[Topped[Boolean]]): Join[TypeRelation] with {

  def join(rv: TypeRelation, other: TypeRelation): TypeRelation =
    if (rv.cols != other.cols)
      throw new IllegalArgumentException("Schemas must match for join")
    (rv, other) match
      case (TypeRelation.Empty(_), _) => other
      case (_, TypeRelation.Empty(_)) => rv
      case (rv: TypeRelation.NonEmpty, other: TypeRelation.NonEmpty) =>
        val newTypes = rv.rows.zip(other.rows).map(joinTypeValue)
        //val newEmpty = boolOps.and(rv.empty, other.empty)
        val newEmpty = (rv.empty, other.empty) match
          case (Topped.Actual(true), Topped.Actual(true)) => Topped.Actual(true)
          case (Topped.Actual(false), Topped.Actual(false)) => Topped.Actual(false)
          case _ => Topped.Top
        TypeRelation(rv.cols, newTypes, newEmpty)

  override def apply(v1: TypeRelation, v2: TypeRelation): MaybeChanged[TypeRelation] =
    // natural join with same columns is an intersection
    MaybeChanged(join(v1, v2), v1)
}