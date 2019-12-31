package org.inca.diff

import org.inca.diff.DiffData.{Context, Patch}

import scala.language.implicitConversions

class Differ[T <: Diffable[T]](_src: T) {

  var src: T = _src
  val oracleFact: DiffableCryptoHashOracle = new DiffableCryptoHashOracle(src)

  def diff(dest: T)(): Patch[T] = {
    val oracle = oracleFact.mkNextOracle(dest)

    // changeTree
    val delCtx = src.extract(oracle)
    val insCtx = dest.extract(oracle)

    // postprocess
    val okvars = delCtx.freevars intersect insCtx.freevars
    val postDel = delCtx.retainMetaVars(okvars, src)
    val postIns = insCtx.retainMetaVars(okvars, dest)

    src = dest

    // diff
    postDel.greatestCommonClosedPrefix(postIns)
  }
}
