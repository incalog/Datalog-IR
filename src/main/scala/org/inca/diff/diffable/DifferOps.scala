package org.inca.diff.diffable

import org.inca.diff.diffable.DiffData.Patch

class DifferOps[T <: Diffable[T]](differ: Differ[T], src: T) {
  def compareTo(dest: T)(implicit mkOracle: MkDiffableOracle[T]): Patch[T] =
    differ.diff(src, dest)

  def applyPatch(p: Patch[T]): Option[T] =
    differ.applyPatch(p, src)
}
