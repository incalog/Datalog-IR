package inca.ir.analysis.base.values

import inca.ir.analysis.RelationOps
import sturdy.effect.EffectStack
import sturdy.effect.failure.Failure
import sturdy.values.Join
import sturdy.values.booleans.BooleanOps
import sturdy.values.ordering.EqOps

class ARelationValueOps[V, B](using effects: EffectStack, joinV: Join[V], booleanOps: BooleanOps[B], eqOps: EqOps[V, B], failure: Failure)
  extends RelationOps[V, B, ARelationValue[V]]:

  type RV = ARelationValue[V]

  override def isEmpty(rv: ARelationValue[V]): B = ???

  override def hasColumn(rv: ARelationValue[V], column: String): Boolean = ???

  override def columns(rv: RV): Seq[String] = ???

  override def unit: ARelationValue[V] = ???

  override def make(cols: Seq[String], vals: Seq[Row]): ARelationValue[V] = ???

  override def rename(rv: ARelationValue[V], subst: Map[String, String]): ARelationValue[V] = ???

  override def project(rv: ARelationValue[V], cols: Seq[String]): ARelationValue[V] = ???

  override def projectAndRename(rv: ARelationValue[V], subst: Map[String, String]): ARelationValue[V] = ???
  
  override def filter(rv: ARelationValue[V])(f: Row => B): ARelationValue[V] = ???
  
  override def map(rv: ARelationValue[V], columnName: String)(f: Row => V): ARelationValue[V] = ???

  override def flatMap(rv: ARelationValue[V])(f: Row => ARelationValue[V]): ARelationValue[V] = ???
  
  override def naturalJoin(rv: ARelationValue[V], other: ARelationValue[V]): ARelationValue[V] = ???

  override def antiJoin(rv: ARelationValue[V], other: ARelationValue[V]): ARelationValue[V] = ???