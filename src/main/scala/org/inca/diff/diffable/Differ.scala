package org.inca.diff.diffable

import org.inca.diff.diffable.DiffData.{Context, Patch}
import org.inca.diff.diffable.Diffable.{ApplyDiffFailed, GreatestCommonPrefixFailed}

class Differ[T] {
  def diff(t1: T with Diffable[T], t2: T with Diffable[T])(implicit mkOracle: MkDiffableOracle): Patch[T] = {
    val oracle = mkOracle(t1, t2)

    // changeTree
    val delCtx = t1.extract(oracle)
    val insCtx = t2.extract(oracle)

    // postprocess
    val okvars = delCtx.freevars intersect insCtx.freevars
    val postDel = delCtx.retainMetaVars(okvars, t1)
    val postIns = insCtx.retainMetaVars(okvars, t2)

    // diff
    greatestCommonClosedPrefix(postDel, postIns)
  }

  private def greatestCommonClosedPrefix(t1: Context[T], t2: Context[T]): Patch[T] =
    try t1.greatestCommonClosedPrefix(t2) catch {
      case GreatestCommonPrefixFailed() => sys.error(s"Unclosable change ${Change(t1, t2)}")
      case e: Throwable => throw e
    }

  final def applyPatch(p: Patch[T], t: T): Option[T] =
    try Some(p.applyPatchTo(t)) catch {
      case ApplyDiffFailed() => None
      case e: Throwable => throw e
    }

  implicit def withDifferOps(t: T with Diffable[T]): DifferOps[T] = new DifferOps[T](this, t)
}
