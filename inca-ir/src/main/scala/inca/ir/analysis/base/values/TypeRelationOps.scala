package inca.ir.analysis.base.values

import inca.ir.Type
import inca.ir.analysis.RelationOps
import sturdy.data.CombineEquiSeq
import sturdy.values
import sturdy.values.MaybeChanged.Unchanged
import sturdy.values.{Changed, Join, MaybeChanged, Topped, Widening}

enum AbsType:
  case Bottom
  case AType(ty: Type)

given JoinAbsType(using Join[Type]): Join[AbsType] with
  import AbsType.*
  override def apply(v1: AbsType, v2: AbsType): MaybeChanged[AbsType] = (v1, v2) match
    case (_, Bottom) => Unchanged(Bottom)
    case (Bottom, _) => Changed(v2)
    case (AType(ty1), AType(ty2)) => Join(ty1, ty2).map(AType.apply)


case class TypeRelation(cols: Seq[String], rows: Seq[AbsType], empty: Topped[Boolean])

class TypeRelationOps(using Join[Type]) extends RelationOps[AbsType, Topped[Boolean], TypeRelation]:
  override def unit: TypeRelation = TypeRelation(Seq(), Seq(), Topped.Actual(true))
  override def isEmpty(rv: TypeRelation): Topped[Boolean] = rv.empty

  override def hasColumn(rv: TypeRelation, column: String): Boolean = rv.cols.contains(column)
  override def columns(rv: TypeRelation): Seq[String] = rv.cols

  override def make(cols: Seq[String], vals: Seq[Row]): TypeRelation =
    val joinRows = new CombineEquiSeq[AbsType, Widening.No]
    val types = vals.foldLeft(cols.map(_ => AbsType.Bottom))((v1,v2) => joinRows(v1, v2).get)
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

  override def map(rv: TypeRelation, columnName: String)(f: Row => AbsType): TypeRelation =
    TypeRelation(rv.cols :+ columnName, rv.rows :+ f(rv.rows), rv.empty)

  override def flatMap(rv: TypeRelation)(f: Row => TypeRelation): TypeRelation =
    naturalJoin(rv, f(rv.rows))

  override def filter(rv: TypeRelation)(f: Row => Topped[Boolean]): TypeRelation =
    f(rv.rows) match
      case Topped.Top => rv.copy(empty = Topped.Top)
      case Topped.Actual(b) => rv // unchanged
      case Topped.Actual(false) => rv.copy(empty = Topped.Actual(true)) // definitely empty

  override def naturalJoin(rv: TypeRelation, other: TypeRelation): TypeRelation =
    val (sameCols, otherNewCols) = other.cols.partition(rv.cols.contains)
    ???

  override def antiJoin(rv: TypeRelation, other: TypeRelation): TypeRelation = ???


