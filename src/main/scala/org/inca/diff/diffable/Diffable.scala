package org.inca.diff.diffable

import org.inca.diff.HasCryptoHash
import org.inca.diff.diffable.DiffData.{Context, Patch, VarMap}
import org.inca.diff.diffable.Diffable.{ApplyDiffFailed, GreatestCommonPrefixFailed}

trait Diffable[T] extends HasCryptoHash {
  val freevars: Set[MetaVar[_]]
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
}

//trait DiffableByMacro[A] extends DiffableInternal[A] {
//  val ops: DiffableImplByMacro[A]
//
//  override val freevars: Set[MetaVar] = ops.freevars
//  override def visitDiffable(f: Diffable => Unit): Unit = ops.visitDiffable(f)
//  override def _extract(oracle: DiffableOracle): A = ops._extract(oracle)
//  override def _retainMetaVars(vs: Set[MetaVar], orig: Diffable): A = ops._retainHoles(vs, orig)
//  override def _greatestCommonClosedPrefix(other: A): A = ops._greatestCommonClosedPrefix(other)
//  override def _applyPatchToOrFail(p: A): A = ops._applyPatchToOrFail(p)
//  override def _delOrFail(other: A, m: VarMap): VarMap = ops._delOrFail(other, m)
//  override def _insOrFail(m: VarMap): A = ops._insOrFail(m)
//}
//object DiffableByMacro {
//  case class DiffableImplByMacro[A](
//    freevars: Set[MetaVar],
//    visitDiffable: (Diffable => Unit) => Unit,
//    _extract: DiffableOracle => A,
//    _retainHoles: (Set[MetaVar], Diffable) => A,
//    _greatestCommonClosedPrefix: A => A,
//    _applyPatchToOrFail: A => A,
//    _delOrFail: (A, VarMap) => VarMap,
//    _insOrFail: VarMap => A
//  )
//}


object Diffable {
  case class GreatestCommonPrefixFailed() extends Exception
  case class ApplyDiffFailed() extends Exception
}

trait DiffableForeach {
  def apply[T <: Diffable[_]](t: T): Unit
}