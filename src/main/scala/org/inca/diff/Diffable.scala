package org.inca.diff

import org.inca.diff.DiffData.{Context, Patch, VarMap}
import org.inca.diff.changeset.SimpleChangesetApi._

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

  def size: Int
  def changeSize: Int

  def computeChangeset(parent: NodeRef, link: Link, other: Context[T], changes: ChangesetBuffer): Unit = ???
  def unload(changes: ChangesetBuffer): Unit = ???
  def load(changes: ChangesetBuffer, forceClone: Boolean): NodeRef = ???
  def ref: NodeRef = URI(toString)

  final def compareTo(other: T): Patch[T] =
    new Differ[T](this).diff(other)

  final def changeset(other: T): Changeset =
    new Differ[T](this).diffChangeset(other)

  final def applyPatch(p: Patch[T]): Option[T] =
    try Some(p.applyPatchTo(this)) catch {
      case ApplyDiffFailed() => None
      case e: Throwable => throw e
    }
}

case class GreatestCommonPrefixFailed() extends Exception
case class ApplyDiffFailed() extends Exception

trait DiffableForeach {
  def apply[T <: Diffable[_]](t: T): Unit
}