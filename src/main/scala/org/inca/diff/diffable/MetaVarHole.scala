package org.inca.diff.diffable

import org.inca.diff.HasCryptoHash
import org.inca.diff.diffable.DiffData.{Context, Patch, VarMap}
import org.inca.diff.diffable.Diffable.ApplyDiffFailed

trait MetaVarHole[T <: Diffable[T]] extends Diffable[T] {
  val mv: MetaVar[T]
  def mkChangeHole: Change[T] => Patch[T]
  def lifted: Context[T]

  override lazy val $hash: Nothing =
    throw new IllegalStateException(s"Input trees may not contain hole $this")

  override lazy val freevars: Set[MetaVar[_]] = Set(mv)

  override def extract(oracle: DiffableOracle): Nothing =
    throw new IllegalStateException(s"Input trees may not contain hole $this")

  override def foreach(f: DiffableForeach): Unit =
    f(this)

  def retainMetaVars(vs: Set[MetaVar[_]], orig: Context[T]): Context[T] = {
    if (vs.contains(mv)) this.lifted else orig
  }

  override def greatestCommonClosedPrefix(other: Context[T]): Patch[T] =
    ChangeHole.mkClosedChangeHole(this.lifted, other, mkChangeHole)

  override def applyPatchTo(t: T): T =
    throw new IllegalStateException(s"Patch may not contain MetaVar holes")

  override def matchTree(other: T): Unit = {
    if (mv.tree == null)
      mv.tree = other
    else if (mv.tree != other)
      throw ApplyDiffFailed()
  }

  override def buildTree(): T =
    if (mv.tree != null)
      mv.tree
    else
      throw ApplyDiffFailed()

  override def toString: String = mv.toString
}