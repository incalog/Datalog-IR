package inca.ir.analysis.base.values

import inca.ir.analysis.RelationOps
import sturdy.values.{Changed, Finite, Join, MaybeChanged, Unchanged, Widen}

import scala.collection

// Schema: Map[C -> V]
// Special cases (rows):
case class CRelationValue[V](cols: Seq[String], rows: Set[Seq[V]]):
  def size: Int = rows.size

class FiniteCRelationValue[V] extends Finite[CRelationValue[V]]