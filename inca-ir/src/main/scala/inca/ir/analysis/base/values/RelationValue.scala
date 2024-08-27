package inca.ir.analysis.base.values

import sturdy.values.{Changed, Finite, Join, MaybeChanged, Unchanged, Widen}


case class RelationValue(columns: Vector[String], values: Vector[Value]):
  def hasColumns: Boolean = columns.nonEmpty
  def hasValues: Boolean = values.nonEmpty
  def hasNoColumns: Boolean = columns.isEmpty
  def hasNoValues: Boolean = values.isEmpty
  def valueMap: Map[String, Value] = columns.zip(values).toMap

  def containsColumns(cols: Vector[String]): Boolean =
    columns.forall(s => cols.contains(s))

given FiniteRV: Finite[RelationValue] with {}

given JoinRV(using j: Join[Value]): Join[RelationValue] with
  override def apply(v1: RelationValue, v2: RelationValue): MaybeChanged[RelationValue] =
    if v1 == v2 then
      Unchanged(v1)
    else if v1.columns.forall(s => v2.columns.contains(s)) && v2.columns.forall(s => v1.columns.contains(s)) then
      if v1.values.length != v2.values.length then // one is the empty Table
        Changed(RelationValue(v1.columns, v1.columns.map(_ => Top)))
      else
        val map1 = v1.columns.zip(v1.values).toMap
        val map2 = v2.columns.zip(v2.values).toMap
        val joins = v1.columns.map(s => j(map1(s), map2(s)))
        if joins.exists(_.hasChanged) then
          Changed(RelationValue(v1.columns, joins.map(_.get)))
        else
          Unchanged(v1)
    else if v1.columns.isEmpty then
      Changed(v2)
    else if v2.columns.isEmpty then
      Changed(v1)
    else
      val newCols = v1.columns.appendedAll(v2.columns.filter(s => !v1.columns.contains(s)))
      val map1 = v1.columns.zip(v1._2).toMap
      val map2 = v2.columns.zip(v2._2).toMap
      val joins = newCols.map(s => j(map1.getOrElse(s, Top), map2.getOrElse(s, Top)).get)
      Changed(RelationValue(newCols, joins))
