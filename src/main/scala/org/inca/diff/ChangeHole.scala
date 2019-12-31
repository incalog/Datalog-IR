package org.inca.diff

import org.inca.diff.DiffData.{Context, Patch, VarMap}

trait ChangeHole[T <: Diffable[T]] extends Diffable[T] { this: T =>
  val change: Change[T]
  def lifted: Patch[T]

  override lazy val $hash: Nothing =
    throw new IllegalStateException(s"Input trees may not contain hole $this")

  override lazy val freevars: Set[MetaVar[_]] =
    change.freevars

  override def extract(oracle: DiffableOracle): Nothing =
    throw new IllegalStateException(s"Input trees may not contain hole $this")

  override def foreach(f: DiffableForeach): Unit =
    f(this)

  def retainMetaVars(vs: Set[MetaVar[_]], orig: Context[T]): Context[T] =
    throw new IllegalStateException(s"Cannot apply change to a change")

  override def greatestCommonClosedPrefix(other: Context[T]): Patch[T] =
    throw new IllegalStateException(s"Cannot create patch of patches")

  override def applyPatchTo(t: T): T = {
    change.delCtx.matchTree(t)
    change.insCtx.buildTree()
  }

  override def matchTree(other: T): Unit = {
    throw new IllegalStateException(s"Cannot apply change to a change")
  }

  override def buildTree(): T =
    throw new IllegalStateException(s"Cannot apply change to a change")

  override def toString: String = change.toString

  override def size: Int = 1 + change.delCtx.changeSize + change.insCtx.changeSize

  override def changeSize: Int = throw new IllegalStateException(s"Input trees may not contain hole $this")
}

object ChangeHole {
  @throws(classOf[GreatestCommonPrefixFailed])
  final def mkClosedChangeHole[T <: Diffable[T]](delCtx: Context[T], insCtx: Context[T], makeChangeHole: Change[T]=>Patch[T], ex: GreatestCommonPrefixFailed=GreatestCommonPrefixFailed()): Patch[T] = {
    val change = Change(delCtx, insCtx)
    if (!change.isClosed) {
      throw ex
    }

    makeChangeHole(change)
  }
}