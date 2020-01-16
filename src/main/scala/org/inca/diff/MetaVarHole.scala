package org.inca.diff

import org.inca.diff.DiffData.{Context, Patch}
import org.inca.diff.changeset.Changeset
import org.inca.diff.changeset.Changeset._

import scala.collection.mutable.ArrayBuffer

trait MetaVarHole[T <: Diffable[T]] extends Diffable[T] { this: T =>
  val mv: MetaVar[T]
  def mkChangeHole: Change[T] => Patch[T]
  def lifted: Context[T]

  override lazy val $hash: Array[Byte] =
    mv.tree.$hash

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

  override def findMinimalClosedChanges(other: Context[T], changes: ArrayBuffer[Change[_]]): Unit =
    ChangeHole.addClosedChange(this.lifted, other, changes)

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

  override def load(changes: ChangesetBuffer, forceClone: Boolean): NodeRef =
    if (forceClone || mv.moved)
      mv.tree.load(changes, forceClone = true)
    else {
      mv.moved = true
      mv.tree.ref
    }

  override def unload(changes: ChangesetBuffer): Unit = {
    changes.buf += DetachNode(mv.tree.ref)
  }

  override def computeChangeset(parent: NodeRef, link: Link, other: Context[T], changes: ChangesetBuffer): Unit =
    if (this != other) {
      this.unload(changes)
      changes += AttachNode(parent, link, other.load(changes, false))
    }
}
