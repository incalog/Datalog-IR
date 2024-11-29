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

case class TypeRelation(cols: Seq[String], rows: Seq[TypeValue], empty: Topped[Boolean])

class TypeRelationOps extends RelationOps[TypeValue, Topped[Boolean], TypeRelation]:
  override def unit: TypeRelation = TypeRelation(Seq(), Seq(), Topped.Actual(true))
  override def isEmpty(rv: TypeRelation): Topped[Boolean] = rv.empty

  override def hasColumn(rv: TypeRelation, column: String): Boolean = rv.cols.contains(column)
  override def columns(rv: TypeRelation): Seq[String] = rv.cols

  override def make(cols: Seq[String], vals: Seq[Row]): TypeRelation =
    val types = vals.foldLeft(cols.map(_ => TypeValue.Bottom))((v1, v2) => v1.zip(v2).map((t1, t2) => t1.join(t2)))
    TypeRelation(cols, types, Topped.Actual(vals.isEmpty))

  override def rename(rv: TypeRelation, subst: Map[String, String]): TypeRelation =
    val newColumns = rv.cols.map(c => subst.getOrElse(c, c))
    TypeRelation(newColumns, rv.rows, rv.empty)

  override def project(rv: TypeRelation, newColumns: Seq[String]): TypeRelation =
    val colsIndex = newColumns.map(rv.cols.indexOf)
    val newRows = colsIndex.map(rv.rows)
    TypeRelation(newColumns, newRows, rv.empty)

  override def projectAndRename(rv: TypeRelation, subst: Map[String, String]): TypeRelation =
    rename(project(rv, subst.keys.toSeq), subst)

  override def map(rv: TypeRelation, columnName: String)(f: Row => TypeValue): TypeRelation =
    TypeRelation(rv.cols :+ columnName, rv.rows :+ f(rv.rows), rv.empty)

  override def flatMap(rv: TypeRelation)(f: Row => TypeRelation): TypeRelation =
    naturalJoin(rv, f(rv.rows))

  override def filter(rv: TypeRelation)(f: Row => Topped[Boolean]): TypeRelation =
    f(rv.rows) match
      case Topped.Top => rv.copy(empty = Topped.Top)
      case Topped.Actual(true) => rv // unchanged
      case Topped.Actual(false) => rv.copy(empty = Topped.Actual(true)) // definitely empty

  override def naturalJoin(rv: TypeRelation, other: TypeRelation): TypeRelation =
    val rvCols = rv.cols.zipWithIndex.toMap
    val otherCols = other.cols.zipWithIndex.toMap

    val newCols = rv.cols ++ other.cols.filterNot(rv.cols.contains)
    val newTypes = for (c <- newCols) yield {
      (rvCols.get(c), otherCols.get(c)) match
        case (Some(rvIx), None) => rv.rows(rvIx)
        case (None, Some(otherIx)) => other.rows(otherIx)
        case (Some(rvIx), Some(otherIx)) => rv.rows(rvIx).meet(other.rows(otherIx))
        case (None, None) => throw new IllegalStateException()
    }
    val newEmpty = (rv.empty, other.empty) match
      case (Topped.Actual(true), _) | (_, Topped.Actual(true)) => Topped.Actual(true)
      case _ => Topped.Top
    TypeRelation(newCols, newTypes, newEmpty)

  override def antiJoin(rv: TypeRelation, other: TypeRelation): TypeRelation =
    val newEmpty = other.empty match
      case Topped.Actual(true) => rv.empty
      case _ => Topped.Top
    TypeRelation(rv.cols, rv.rows, newEmpty)


