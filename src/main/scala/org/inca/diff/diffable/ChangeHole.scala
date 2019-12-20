package org.inca.diff.diffable

import org.inca.diff.HasCryptoHash
import org.inca.diff.diffable.DiffData.{Context, Patch, VarMap}
import org.inca.diff.diffable.Diffable.GreatestCommonPrefixFailed

trait ChangeHole[T] extends Diffable[T] {
  val change: Change[T]
  def lifted: Patch[T]

  override lazy val $hash: Nothing =
    throw new IllegalStateException(s"Input trees may not contain hole $this")

  override lazy val freevars: Set[MetaVar] =
    change.freevars

  override def extract(oracle: DiffableOracle): Nothing =
    throw new IllegalStateException(s"Input trees may not contain hole $this")

  override def initOracle(f: HasCryptoHash => Unit): Unit =
    f(this.lifted)

  def retainMetaVars(vs: Set[MetaVar], orig: Context[T]): Context[T] =
    throw new IllegalStateException(s"Cannot apply change to a change")

  override def greatestCommonClosedPrefix(other: Context[T]): Patch[T] =
    throw new IllegalStateException(s"Cannot create patch of patches")

  override def applyPatchTo(t: T): T = {
    val m = change.delCtx.del(t, Map())
    change.insCtx.ins(m).asInstanceOf[T]
  }

  override def del(other: T, m: VarMap[T]): VarMap[T] = {
    throw new IllegalStateException(s"Cannot apply change to a change")
  }

  override def ins(m: VarMap[T]): T =
    throw new IllegalStateException(s"Cannot apply change to a change")

  override def toString: String = change.toString
}

object ChangeHole {
  @throws(classOf[GreatestCommonPrefixFailed])
  final def mkClosedChangeHole[T](delCtx: Context[T], insCtx: Context[T], makeChangeHole: Change[T]=>Patch[T]): Patch[T] = {
    val change = Change(delCtx, insCtx)
    if (change.isClosed)
      makeChangeHole(change)
    else
      throw GreatestCommonPrefixFailed()
  }
}