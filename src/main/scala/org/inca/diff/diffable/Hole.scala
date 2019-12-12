package org.inca.diff.diffable

import org.inca.diff.diffable.DiffData.{Patch, VarMap}
import org.inca.diff.diffable.Diffable.{ApplyDiffFailed, DeletionFailedException, InsertionFailedException}

trait Hole[A <: Diffable] extends Diffable {
  val a: Plug
  def mkHole: Plug => A

  override lazy val $hash: Nothing =
    throw new IllegalStateException(s"Input trees may not contain hole $this")

  override lazy val freevars: Set[MetaVar] =
    a.freevars

  override def _extract(oracle: DiffableOracle): Nothing =
    throw new IllegalStateException(s"Input trees may not contain hole $this")

  override def visitDiffable(f: Diffable => Unit): Unit =
    f(this)

  def _retainHoles(vs: Set[_], orig: Diffable): A =
    if (vs.asInstanceOf[Set[Plug]].contains(a)) this.asInstanceOf[A] else orig.asInstanceOf[A]

  override def _greatestCommonClosedPrefix(other: Diffable): A =
    mkPrefix(this, other, mkHole)

  override def _applyPatchToOrFail(p: Diffable): A =
    p.applyChange(a.asInstanceOf[Change]).getOrElse(throw ApplyDiffFailed()).asInstanceOf[A]

  override def _delOrFail(other: Diffable, m: VarMap): VarMap = {
    val mv = a.asInstanceOf[MetaVar]
    m.get(mv) match {
      case None => m + (mv -> other)
      case Some(t_) => if(other == t_) m else throw DeletionFailedException()
    }
  }

  override def _insOrFail(m: VarMap): A =
    m.getOrElse(a.asInstanceOf[MetaVar], throw InsertionFailedException()).asInstanceOf[A]
}