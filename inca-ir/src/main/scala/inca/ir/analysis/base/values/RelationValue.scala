package inca.ir.analysis.base.values

import sturdy.values.{Changed, Finite, Join, MaybeChanged, Unchanged, Widen}

trait RelationAbstraction[K, V]:
  def columns: Vector[K]
  def values: Vector[V]

  def hasColumns: Boolean = columns.nonEmpty
  def hasValues: Boolean = values.nonEmpty
  def isUnitTable: Boolean = columns.isEmpty
  def isEmptyTable: Boolean = values.isEmpty
  def containsColumns(cols: Vector[K]): Boolean = columns.forall(cols.contains)
  def hasValueForColumn(col: K): Boolean = get(col).isDefined

  private lazy val valueMap: Map[K, V] = columns.zip(values).toMap

  def apply(col: K): V = valueMap(col)
  def get(col: K): Option[V] = valueMap.get(col)
  def getOrElse(col: K, default: => V): V = valueMap.getOrElse(col, default)



case class RelationValue(columns: Vector[String], values: Vector[Value]) extends RelationAbstraction[String, Value]:
  override def toString: String = columns.map { c =>
    val v = (if hasValueForColumn(c) then this(c) else "None")
    s"$c -> $v"
  }.mkString("{", ", ", "}")



class FiniteRV extends Finite[RelationValue]

class JoinRV(using joinValue: Join[Value]) extends Join[RelationValue]:
  override def apply(v1: RelationValue, v2: RelationValue): MaybeChanged[RelationValue] =
    if v1 == v2 then
      Unchanged(v1)
    else if v1.isUnitTable then
      Changed(v2)
    else if v2.isUnitTable then
      Changed(v1)
    else if v1.containsColumns(v2.columns) && v2.containsColumns(v1.columns) then
      if v1.values.length != v2.values.length then // one is the empty table
        Changed(RelationValue(v1.columns, v1.columns.map(_ => Top)))
      else
        val joins = v1.columns.map(s => joinValue(v1(s), v2(s)))
        if joins.exists(_.hasChanged) then
          Changed(RelationValue(v1.columns, joins.map(_.get)))
        else
          Unchanged(v1)
    else
      val newCols = v1.columns.appendedAll(v2.columns).distinct
      val joins = newCols.map(s => joinValue(v1.getOrElse(s, Top), v2.getOrElse(s, Top)).get)
      Changed(RelationValue(newCols, joins))
