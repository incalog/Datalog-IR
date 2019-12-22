package org.inca.diff.diffable

import org.inca.diff.diffable.DiffData.Context


object DiffData {
  type Context[T] = T with Diffable[T] // with MetaVarHole[T]
  type Patch[T] = T with Diffable[T] // with ChangeHole[T with MetaVarHole[T]]
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

case class MetaVar[R](i: Int) {
  var tree: R = null.asInstanceOf[R]

  override def toString: String = s"#$i"
}

case class Change[T](delCtx: Context[T], insCtx: Context[T]) {
  lazy val freevars: Set[MetaVar[_]] = insCtx.freevars diff delCtx.freevars
  def isClosed: Boolean = freevars.isEmpty

  override def toString: String = s"($delCtx -> $insCtx)"
}
