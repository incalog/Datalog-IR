package inca.ir.analysis.base.values

import inca.ir.analysis.RelationOps
import sturdy.effect.failure.Failure

class CRelationValueOps[V](using failure: Failure)
  extends RelationOps[V, Boolean, CRelationValue[V]]:

  type RV = CRelationValue[V]

  override def unit: CRelationValue[V] = CRelationValue(Seq(), Set(Seq()))
  
  override def isEmpty(rv: CRelationValue[V]): Boolean = rv.rows.isEmpty

  override def hasColumn(rv: RV, column: String): Boolean = rv.cols.contains(column)

  override def columns(rv: RV): Seq[String] = rv.cols
  
  override def make(cols: Seq[String], vals: Seq[Row]): CRelationValue[V] = CRelationValue(cols, vals.toSet)

  override def rename(rv: CRelationValue[V], subst: Map[String, String]): CRelationValue[V] =
    rv.rename(subst)
  
  override def project(rv: CRelationValue[V], cols: Seq[String]): CRelationValue[V] =
    rv.project(cols)

  override def projectAndRename(rv: CRelationValue[V], subst: Map[String, String]): CRelationValue[V] =
    rv.projectAndRename(subst)
  
  override def filter(rv: CRelationValue[V])(f: Row => Boolean): CRelationValue[V] =
    rv.filter(f)

  override def map(rv: CRelationValue[V], columnName: String)(f: Row => V): CRelationValue[V] =
    rv.map(columnName)(f)

  override def flatMap(rv: CRelationValue[V])(f: Row => CRelationValue[V]): CRelationValue[V] =
    assert(!rv.isEmpty)
    val generated = rv.rows.map(f).reduce(_.union(_))
    rv.naturalJoin(generated)

  override def naturalJoin(rv: CRelationValue[V], other: CRelationValue[V]): CRelationValue[V] =
    rv.naturalJoin(other)

  override def antiJoin(rv: CRelationValue[V], other: CRelationValue[V]): CRelationValue[V] =
    rv.antiJoin(other)



