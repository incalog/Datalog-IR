package org.inca.diff.diffable

import org.inca.diff.WithCachedCryptoHash
import org.inca.diff.diffable.DiffData.{DiffableContext, DiffableNoHoles, Patch, VarMap}
import org.inca.diff.diffable.Diffable.{ApplyDiffFailed, DeletionFailedException, DiffableData, GreatestCommonPrefixFailed, InsertionFailedException}

trait Diffable extends WithCachedCryptoHash {

  val freevars: Set[MetaVar]
  def isClosed: Boolean = freevars.isEmpty

  def visitDiffable(f: Diffable => Unit): Unit

  def _extract(oracle: DiffableOracle): Diffable
  def extract(oracle: DiffableOracle): DiffableContext =
  _extract(oracle)

  def _retainHoles(vs: Set[_], orig: Diffable): Diffable
  final def retainHoles(vs: Set[_], orig: DiffableNoHoles): DiffableContext =
  _retainHoles(vs, orig)

  @throws(classOf[GreatestCommonPrefixFailed])
  def _greatestCommonClosedPrefix(other: Diffable): Diffable
  def greatestCommonClosedPrefix(other: Diffable): Patch =
    try _greatestCommonClosedPrefix(other) catch {
      case GreatestCommonPrefixFailed() => sys.error(s"Unclosable change ${Change(this, other)}")
      case e: Throwable => throw e
    }

  def compareTo(other: DiffableNoHoles)(implicit mkOracle: MkDiffableOracle): Patch = {
    val oracle = mkOracle(this, other)

    // changeTree
    val delCtx = this.extract(oracle)
    val insCtx = other.extract(oracle)

    // postprocess
    val okvars = delCtx.freevars intersect insCtx.freevars
    val postDel = delCtx.retainHoles(okvars, this)
    val postIns = insCtx.retainHoles(okvars, other)

    // diff
    postDel.greatestCommonClosedPrefix(postIns)
  }

  @throws(classOf[ApplyDiffFailed])
  def _applyPatchToOrFail(p: Diffable): Diffable

  final def applyPatch(p: Patch): Option[DiffableNoHoles] =
    try Some(p._applyPatchToOrFail(this)) catch {
      case ApplyDiffFailed() => None
      case e: Throwable => throw e
    }

  final def applyChange(change: Change): Option[DiffableNoHoles] =
    change.delCtx.del(this, Map()) flatMap change.insCtx.ins

  @throws(classOf[DeletionFailedException])
  def _delOrFail(other: Diffable, m: VarMap): VarMap

  def del(other: DiffableNoHoles, m: VarMap): Option[VarMap] =
    try Some(_delOrFail(other, m)) catch {
      case DeletionFailedException() => None
      case e: Throwable => throw e
    }

  @throws(classOf[InsertionFailedException])
  def _insOrFail(m: VarMap): Diffable

  final def ins(m: VarMap): Option[DiffableNoHoles] =
    try Some(_insOrFail(m).asInstanceOf[DiffableNoHoles]) catch {
      case InsertionFailedException() => None
      case e: Throwable => throw e
    }

  @throws(classOf[GreatestCommonPrefixFailed])
  final def mkPrefix[A](delCtx: Diffable, insCtx: Diffable, makeChangeHole: Change=>A): A = {
    val change = Change(delCtx, insCtx)
    if (change.isClosed)
      makeChangeHole(change)
    else
      throw GreatestCommonPrefixFailed()
  }
}

object Diffable {
  type DiffableData[A <: Plug] = Diffable

  case class GreatestCommonPrefixFailed() extends Exception
  case class ApplyDiffFailed() extends Exception
  case class InsertionFailedException() extends Exception
  case class DeletionFailedException() extends Exception
}
