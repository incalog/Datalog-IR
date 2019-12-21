package org.inca.diff.diffable

import org.inca.diff.diffable.DiffData.Context


object DiffData {
  type Context[T] = T with Diffable[T] // with MetaVarHole[T]
  type Patch[T] = T with Diffable[T] // with ChangeHole[T with MetaVarHole[T]]
  type VarMap[T] = Map[MetaVar[T], T]
}

trait Plug {
  val freevars: Set[MetaVar[_]]
}

case class MetaVar[R](val i: Int) extends Plug {
  override val freevars: Set[MetaVar[_]] = Set(this)

  var tree: R = null.asInstanceOf[R]

  override def toString: String = s"#$i"
}

case class Change[T](delCtx: Context[T], insCtx: Context[T]) extends Plug {
  override lazy val freevars: Set[MetaVar[_]] = insCtx.freevars diff delCtx.freevars
  def isClosed: Boolean = freevars.isEmpty

  override def toString: String = s"($delCtx -> $insCtx)"
}
