package org.inca.diff

import org.inca.diff.DiffData.{Context, Patch, VarMap}

trait Diffable[T <: Diffable[T]] extends HasCryptoHash {
  def freevars: Set[MetaVar[_]]
  def isClosed: Boolean = freevars.isEmpty

  def foreach(f: DiffableForeach): Unit
  def extract(oracle: DiffableOracle): Context[T]
  def retainMetaVars(vs: Set[MetaVar[_]], orig: Context[T]): Context[T]

  @throws(classOf[GreatestCommonPrefixFailed])
  def greatestCommonClosedPrefix(other: Context[T]): Patch[T]

  @throws(classOf[ApplyDiffFailed])
  def applyPatchTo(t: T): T

  @throws(classOf[ApplyDiffFailed])
  def matchTree(other: T): Unit

  @throws(classOf[ApplyDiffFailed])
  def buildTree(): T

  def size: Int
  def changeSize: Int

  final def compareTo(other: T): Patch[T] =
    Differ.diff(this.asInstanceOf[T], other)

  final def applyPatch(p: Patch[T]): Option[T] =
    Differ.applyPatch(p, this.asInstanceOf[T])
}

case class GreatestCommonPrefixFailed() extends Exception
case class ApplyDiffFailed() extends Exception

trait DiffableForeach {
  def apply[T <: Diffable[_]](t: T): Unit
}