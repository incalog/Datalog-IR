package inca.ir.execution

import inca.util.Tabulator

import scala.collection.mutable.ListBuffer

trait RelationUpdateListener(val rel: Relation):
  def tupleAdded(tup: rel.Tuple): Unit

  def tupleRemoved(tup: rel.Tuple): Unit

class DeltaRelationConstructor(r: Relation) extends RelationUpdateListener(r):
  val added: ListBuffer[rel.Tuple] = ListBuffer.empty
  val removed: ListBuffer[rel.Tuple] = ListBuffer.empty

  override def tupleAdded(tup: rel.Tuple): Unit = added += tup

  override def tupleRemoved(tup: rel.Tuple): Unit = removed += tup

  override def toString: String =
    s"DeltaRelation(${rel.name}, ${added.size} added, ${removed.size} removed)"

  def asTable: String =
    val plus = Tabulator.format(s"Added to ${rel.name}", rel.parameterNames, added.toSeq.map(rel.flattenEntry))
    val minus = Tabulator.format(s"Removed from ${rel.name}", rel.parameterNames, removed.toSeq.map(rel.flattenEntry))
    plus + "\n" + minus


