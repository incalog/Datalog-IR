package org.inca.diff

import org.inca.diff.DiffData.Patch
import org.inca.diff.changeset.SimpleChangesetApi

import scala.collection.mutable.ArrayBuffer
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

  def diffChangeset(dest: T)(): SimpleChangesetApi.Changeset = {
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
    val buf = ArrayBuffer[SimpleChangesetApi.ChangeCmd]()
    postDel.computeChangeset(
      SimpleChangesetApi.URI("<root>"),
      SimpleChangesetApi.RootLink,
      postIns,
      new SimpleChangesetApi.ChangesetBuffer(buf))
    buf.toList

  }
}
