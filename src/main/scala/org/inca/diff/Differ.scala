package org.inca.diff

import org.inca.diff.DiffData.{Context, Patch}

import scala.language.implicitConversions

object Differ {
  def diff[T <: Diffable[T]](t1: T, t2: T, mkOracle: MkDiffableOracle=DiffableCryptoHashOracle): Patch[T] = {
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

  private def greatestCommonClosedPrefix[T <: Diffable[T]](t1: Context[T], t2: Context[T]): Patch[T] =
    try t1.greatestCommonClosedPrefix(t2) catch {
      case GreatestCommonPrefixFailed() => sys.error(s"Unclosable change ${Change(t1, t2)}")
      case e: Throwable => throw e
    }

  final def applyPatch[T <: Diffable[T]](p: Patch[T], t: T): Option[T] =
    try Some(p.applyPatchTo(t)) catch {
      case ApplyDiffFailed() => None
      case e: Throwable => throw e
    }
}
