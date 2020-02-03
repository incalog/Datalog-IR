package org.inca.diff

import org.inca.diff.DiffData.Context


object DiffData {
  type Context[T <: Diffable[_]] = T // with MetaVarHole[T]
  type Patch[T <: Diffable[T]] = T // with ChangeHole[T with MetaVarHole[T]]
  type VarMap[T] = Map[MetaVar[T], T]
}

// T = pure tree
// Context[T] = T, MetaVarHole may occur
// Patch[T] = T, ChangeHole may occur

// data Tree = Node2 T T | Node3 T T T | Leaf String
// class ChangeHole extends Tree
// class MetaVarHole extends Tree

// Node2(Node2(a, b), Node3(c, d, e))
// Node2(Node3(a, b, x), Node3(c, d, y))

// Change
// del = Node2(Node2(#1, #2), Node3(#3, #4, e)) // 1->a, 2->b, 3->c, 4->d
// ins = Node2(Node3(#1, #2, x), Node3(#3, #4, y))

// Diff
// Node2(
//   {Node2(#1, #2)->Node3(#1, #2, x)}, // ChangeHole
//   Node3({#3->#3}, {#4->#4}, {e->y})

case class MetaVar[R](i: Int, @transient var tree: R) {
  var moved: Boolean = false
  override def toString: String = s"#$i"
}

trait Change[T <: Diffable[_]] {
  def freevars: Set[MetaVar[_]]
  def isClosed: Boolean
  def generic: Change[_] = this
}
object Change {
  def makeClosed[T <: Diffable[_]](delCtx: Context[T], insCtx: Context[T]): Option[Change[T]] = {
    val change = make(delCtx, insCtx)
    if (change.isClosed)
      Some(change)
    else
      None
  }
  def make[T <: Diffable[_]](delCtx: Context[T], insCtx: Context[T]): Change[T] = {
    if (delCtx.isInstanceOf[MetaVarHole[_]] && delCtx == insCtx)
      IdentityChange[T]()
    else
      RewriteChange(delCtx, insCtx)
  }
}
case class IdentityChange[T <: Diffable[_]]() extends Change[T] {
  override def freevars: Set[MetaVar[_]] = Set()
  override def isClosed: Boolean = true
  override def toString: String = "#id"
}
case class RewriteChange[T <: Diffable[_]](delCtx: Context[T], insCtx: Context[T]) extends Change[T] {
  lazy val freevars: Set[MetaVar[_]] = insCtx.freevars diff delCtx.freevars
  override def isClosed: Boolean = freevars.isEmpty
  override def toString: String = s"($delCtx -> $insCtx)"
}
