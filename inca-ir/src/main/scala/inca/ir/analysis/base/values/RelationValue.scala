package inca.ir.analysis.base.values

import inca.ir.analysis.RelationOps
import sturdy.values.{Changed, Finite, Join, MaybeChanged, Unchanged, Widen}

import scala.collection

case class RelationValue[C, V](cols: Seq[C], rows: Iterable[Seq[V]], neg: Boolean = false):
  def size: Int = rows.size

class FiniteRV[C, V] extends Finite[RelationValue[C, V]]

class JoinRV[C, V](using joinValue: Join[V]) extends Join[RelationValue[C, V]]:
  private def naturalJoin(v1: RelationValue[C, V], v2: RelationValue[C, V]): RelationValue[C, V] = ???

  def antiJoin(v1: RelationValue[C, V], v2: RelationValue[C, V]): RelationValue[C, V] = ???

  private def join(v1: RelationValue[C, V], v2: RelationValue[C, V]): RelationValue[C, V] = (v1.neg, v2.neg) match
    case (false, false) => naturalJoin(v1, v2)
    case (false, true) => antiJoin(v1, v2)
    case _ => throw IllegalStateException("Invalid sign combination")

  override def apply(v1: RelationValue[C, V], v2: RelationValue[C, V]): MaybeChanged[RelationValue[C, V]] =
    val joined = join(v1, v2)
    if joined == v1 then
      Unchanged(joined)
    else
      Changed(joined)