package inca.ir.analysis.base.values

import inca.ir.Type
import inca.ir.analysis.RelationOps
import sturdy.data.CombineEquiSeq
import sturdy.values
import sturdy.values.MaybeChanged.Unchanged
import sturdy.values.*

enum TypeValue:
  case Bottom
  case AType(ty: Type)
  case Top

  override def toString: String = this match
    case TypeValue.Bottom => "Bottom"
    case TypeValue.AType(ty) => ty.toString
    case TypeValue.Top => "Top"

  def join(that: TypeValue): TypeValue = (this, that) match
    case (_, Bottom) => this
    case (Bottom, _) => that
    case (Top, _) | (_, Top) => Top
    case (AType(ty1), AType(ty2)) => if (ty1 == ty2) this else Top

  def meet(that: TypeValue): TypeValue = (this, that) match
    case (Bottom, _) | (_, Bottom) => Bottom
    case (Top, _) => that
    case (_, Top) => this
    case (AType(ty1), AType(ty2)) => if (ty1 == ty2) this else Bottom

case class TypeRelation(cols: Seq[String], rows: Seq[TypeValue], empty: Topped[Boolean]):
  override def toString: String =
    if (rows.isEmpty)
      s"[${cols.mkString(", ")}, $empty]"
    else
      s"[${cols.zip(rows).toMap.mkString(", ")}, $empty]"

  def rename(subst: Map[String, String]): TypeRelation =
    val newColumns = cols.map(c => subst.getOrElse(c, c))
    TypeRelation(newColumns, rows, empty)

  def project(newColumns: Seq[String]): TypeRelation =
    val colsIndex = newColumns.map(cols.indexOf)
    val newRows = colsIndex.map(rows)
    TypeRelation(newColumns, newRows, empty)

  def map(columnName: String)(f: Seq[TypeValue] => TypeValue): TypeRelation =
    TypeRelation(cols :+ columnName, rows :+ f(rows), empty)

  def flatMap(f: Seq[TypeValue] => TypeRelation): TypeRelation = f(rows) match
    case tr@TypeRelation(_, _, Topped.Actual(true)) =>
      TypeRelation((cols ++ tr.cols).distinct, Seq(), Topped.Actual(true))
    case tr =>
      naturalJoin(tr)

  def filter(f: Seq[TypeValue] => Topped[Boolean]): TypeRelation = f(rows) match
    case Topped.Top => copy(empty = Topped.Top)
    case Topped.Actual(true) => this // unchanged
    case Topped.Actual(false) => copy(empty = Topped.Actual(true)) // definitely empty

  def naturalJoin(other: TypeRelation): TypeRelation =
    val rvCols = cols.zipWithIndex.toMap
    val otherCols = other.cols.zipWithIndex.toMap

    val newCols = cols ++ other.cols.filterNot(cols.contains)
    val newTypes = for (c <- newCols) yield {
      (rvCols.get(c), otherCols.get(c)) match
        case (Some(rvIx), None) => rows(rvIx)
        case (None, Some(otherIx)) => other.rows(otherIx)
        case (Some(rvIx), Some(otherIx)) => rows(rvIx).meet(other.rows(otherIx))
        case (None, None) => throw new IllegalStateException()
    }
    val newEmpty = (empty, other.empty) match
      case (Topped.Actual(true), _) | (_, Topped.Actual(true)) => Topped.Actual(true)
      case _ => Topped.Top
    TypeRelation(newCols, newTypes, newEmpty)

  def antiJoin(other: TypeRelation): TypeRelation =
    val newEmpty = other.empty match
      case Topped.Actual(true) => empty
      case _ => Topped.Top
    TypeRelation(cols, rows, newEmpty)

  def join(other: TypeRelation): TypeRelation =
    val (newCols, newTypes) = if (cols != other.cols) {
      throw new IllegalArgumentException("Schemas must match for join")
      val rvCols = cols.zipWithIndex.toMap
      val otherCols = other.cols.zipWithIndex.toMap
      val newCols = cols ++ other.cols.filterNot(cols.contains)
      val newTypes = for (c <- newCols) yield {
        (rvCols.get(c), otherCols.get(c)) match
          case (Some(rvIx), None) => rows(rvIx)
          case (None, Some(otherIx)) => other.rows(otherIx)
          case (Some(rvIx), Some(otherIx)) => rows(rvIx).join(other.rows(otherIx))
          case (None, None) => throw new IllegalStateException()
      }
      (newCols, newTypes)
    } else {
      (cols, this.rows.zip(other.rows).map(p => p._1.join(p._2)))
    }

    val newEmpty = (empty, other.empty) match
      case (Topped.Actual(true), Topped.Actual(true)) => Topped.Actual(true)
      case (Topped.Actual(false), Topped.Actual(false)) => Topped.Actual(false)
      case _ => Topped.Top
    TypeRelation(newCols, newTypes, newEmpty)

class TypeRelationOps extends RelationOps[TypeValue, Topped[Boolean], TypeRelation]:
  override def isEmpty(rv: TypeRelation): Topped[Boolean] = rv.empty

  override def hasColumn(rv: TypeRelation, column: String): Boolean = rv.cols.contains(column)
  override def columns(rv: TypeRelation): Seq[String] = rv.cols

  override def make(cols: Seq[String], vals: Seq[Row]): TypeRelation =
    val types = vals.foldLeft(cols.map(_ => TypeValue.Bottom))((v1, v2) => v1.zip(v2).map((t1, t2) => t1.join(t2)))
    TypeRelation(cols, types, Topped.Actual(vals.isEmpty))

  override def rename(rv: TypeRelation, subst: Map[String, String]): TypeRelation =
    rv.rename(subst)

  override def project(rv: TypeRelation, newColumns: Seq[String]): TypeRelation =
    rv.project(newColumns)

  override def map(rv: TypeRelation, columnName: String)(f: Row => TypeValue): TypeRelation =
    rv.map(columnName)(f)

  override def flatMap(rv: TypeRelation)(f: Row => TypeRelation): TypeRelation =
    rv.flatMap(f)

  override def filter(rv: TypeRelation)(f: Row => Topped[Boolean]): TypeRelation =
    rv.filter(f)

  override def naturalJoin(rv: TypeRelation, other: TypeRelation): TypeRelation =
    rv.naturalJoin(other)

  override def antiJoin(rv: TypeRelation, other: TypeRelation): TypeRelation =
    rv.antiJoin(other)


given JoinTV: Join[TypeValue] with {
  override def apply(v1: TypeValue, v2: TypeValue): MaybeChanged[TypeValue] =
    MaybeChanged(v1.join(v2), v1)
}

given JoinTRV: Join[TypeRelation] with {
  override def apply(v1: TypeRelation, v2: TypeRelation): MaybeChanged[TypeRelation] =
    // natural join with same columns is an intersection
    MaybeChanged(v1.join(v2), v1)
}