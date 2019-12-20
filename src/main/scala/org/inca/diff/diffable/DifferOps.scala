package org.inca.diff.diffable

import org.inca.diff.diffable.DiffData.Patch

class DifferOps[T](differ: Differ[T], src: T with Diffable[T]) {
  def compareTo(dest: T with Diffable[T])(implicit mkOracle: MkDiffableOracle): Patch[T] =
    differ.diff(src, dest)

  def applyPatch(p: Patch[T]): Option[T] =
    differ.applyPatch(p, src)
}
