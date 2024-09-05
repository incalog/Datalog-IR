package inca.ir.analysis.base.values

import inca.ir.analysis.RelationOps
import sturdy.values.{Changed, Finite, Join, MaybeChanged, Unchanged, Widen}

import scala.collection

case class RelationValue[C, V](cols: Seq[C], rows: Iterable[Seq[V]]):
  def size: Int = rows.size

  def isUnit: Boolean = cols.isEmpty && (rows.size == 1) && rows.head.isEmpty
  def isEmpty: Boolean = rows.isEmpty

class FiniteRV[C, V] extends Finite[RelationValue[C, V]]

class JoinRV[C, V](using joinValue: Join[V]) extends Join[RelationValue[C, V]]:

  private def collapse(v1: RelationValue[C, V]): RelationValue[C, V] = ???

  private def join(v1: RelationValue[C, V], v2: RelationValue[C, V]): RelationValue[C, V] =
    // empty table == top // nope I guess
    // unit table == bot

    // top: RelationValue(X, Seq(Seq(Top)))
    // 

    if v1.isUnit then
      v2
    else if v2.isUnit then
      v1
    else if v1.isEmpty then
      v1
    else if v2.isEmpty then
      v2
    else
      // This is the join on the abstract domain, not the join on the tables! Use relation ops for that
      val haveSameCols = (v1.cols.toSet == v2.cols.toSet)
      if haveSameCols then
        val union = RelationValue(v1.cols, v1.rows ++ v2.rows)
        collapse(union)
      else




  override def apply(v1: RelationValue[C, V], v2: RelationValue[C, V]): MaybeChanged[RelationValue[C, V]] =
    val joined = join(v1, v2)
    if joined == v1 then
      Unchanged(joined)
    else
      Changed(joined)