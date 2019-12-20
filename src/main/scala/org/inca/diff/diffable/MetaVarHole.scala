package org.inca.diff.diffable

import org.inca.diff.HasCryptoHash
import org.inca.diff.diffable.DiffData.{Context, Patch, VarMap}
import org.inca.diff.diffable.Diffable.ApplyDiffFailed

trait MetaVarHole[T] extends Diffable[T] {
  val mv: MetaVar
  def mkChangeHole: Change[T] => Patch[T]
  def lifted: Context[T]

  override lazy val $hash: Nothing =
    throw new IllegalStateException(s"Input trees may not contain hole $this")

  override lazy val freevars: Set[MetaVar] =
    mv.freevars

  override def extract(oracle: DiffableOracle): Nothing =
    throw new IllegalStateException(s"Input trees may not contain hole $this")

  override def initOracle(f: HasCryptoHash => Unit): Unit =
    f(this.lifted)

  def retainMetaVars(vs: Set[MetaVar], orig: Context[T]): Context[T] = {
    if (vs.contains(mv)) this.lifted else orig
  }

  override def greatestCommonClosedPrefix(other: Context[T]): Patch[T] =
    ChangeHole.mkClosedChangeHole(this.lifted, other, mkChangeHole)

  override def applyPatchTo(t: T): T =
    throw new IllegalStateException(s"Patch may not contain MetaVar holes")

  override def del(other: T, m: VarMap[T]): VarMap[T] = {
    m.get(mv) match {
      case None => m + (mv -> other)
      case Some(t_) => if (other == t_) m else throw ApplyDiffFailed()
    }
  }

  override def ins(m: VarMap[T]): T =
    m.getOrElse(mv, throw ApplyDiffFailed())

  override def toString: String = mv.toString
}