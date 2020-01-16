package org.inca.diff

import org.inca.diff.DiffData.{Context, Patch, VarMap}
import org.inca.diff.changeset.ChangesetApi._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait Diffable[T <: Diffable[T]] extends HasCryptoHash { this: T =>
  def freevars: Set[MetaVar[_]]
  def isClosed: Boolean = freevars.isEmpty

  def foreach(f: DiffableForeach): Unit
  def extract(oracle: DiffableOracle): Context[T]
  def retainMetaVars(vs: Set[MetaVar[_]], orig: Context[T]): Context[T]

  @throws(classOf[GreatestCommonPrefixFailed])
  def greatestCommonClosedPrefix(other: Context[T]): Patch[T]

  @throws(classOf[GreatestCommonPrefixFailed])
  def findMinimalClosedChanges(other: Context[T], changes: ArrayBuffer[Change[_]]): Unit

  @throws(classOf[ApplyDiffFailed])
  def applyPatchTo(t: T): T

  @throws(classOf[ApplyDiffFailed])
  def matchTree(other: T): Unit

  @throws(classOf[ApplyDiffFailed])
  def buildTree(): T

  def computeChangeset(parent: NodeRef, link: Link, other: Context[T], changes: ChangesetBuffer): Unit
  def unload(changes: ChangesetBuffer): Unit
  def load(changes: ChangesetBuffer, forceClone: Boolean): NodeRef
  def ref: URI = URI(this.$hashString)

  final def compareTo(other: T): Patch[T] =
    new Differ[T](this).diff(other)

  final def changeset(other: T): Changeset =
    new Differ[T](this).diffChangeset(other)

  final def applyPatch(p: Patch[T]): Option[T] =
    try Some(p.applyPatchTo(this)) catch {
      case ApplyDiffFailed() => None
      case e: Throwable => throw e
    }

  def size: Int = 0
}

object Diffable {
  def computeOptionChangeset[T <: Diffable[T]](srcparent: NodeRef, srclink: Link, src: Option[Diffable[T]], other: Option[Context[T]], changes: ChangesetBuffer): Unit = {
    if (src.isEmpty && other.isEmpty) {
      // nothing
    } else if (src.isEmpty) {
      val newnode = other.get.load(changes, forceClone = false)
      changes += AttachNode(srcparent, srclink, newnode)
    } else if (other.isEmpty) {
      src.get.unload(changes)
    } else {
      src.get.computeChangeset(srcparent, srclink, other.get, changes)
    }
  }

  def computeListChangeset[T <: Diffable[T]](srcparent: NodeRef, srclink: Link, src: Seq[Diffable[T]], other: Seq[Context[T]], changes: ChangesetBuffer): Unit = {
    src.zip(other).zipWithIndex.foreach { case ((srcElem, otherElem), i) =>
      srcElem.computeChangeset(srcparent, ListIndexLink(srclink, i), otherElem, changes)
    }

    val srcSize = src.size
    val otherSize = other.size
    if (srcSize < otherSize)
      // new list is longer than old list, attach additional nodes
      for (i <- srcSize until otherSize) {
        val newnode = other(i).load(changes, forceClone = false)
        changes += AttachNode(srcparent, ListIndexLink(srclink, i), newnode)
      }
    else if (otherSize < srcSize)
      // new list is shorter than old list, detach surplus nodes
      for (i <- otherSize until srcSize) {
        src(i).unload(changes)
      }
  }

  def greatestCommonClosedOptionPrefix[T <: Diffable[T]](src: Option[Diffable[T]], other: Option[Context[T]]): Option[T] = {
    if (src.isEmpty && other.isEmpty)
      None
    else if (src.isEmpty) {
      throw GreatestCommonPrefixFailed()
    } else if (other.isEmpty) {
      throw GreatestCommonPrefixFailed()
    } else {
      val p = src.get.greatestCommonClosedPrefix(other.get)
      Some(p)
    }
  }
}

case class GreatestCommonPrefixFailed() extends Exception
case class ApplyDiffFailed() extends Exception

trait DiffableForeach {
  def apply[T <: Diffable[_]](t: T): Unit
}