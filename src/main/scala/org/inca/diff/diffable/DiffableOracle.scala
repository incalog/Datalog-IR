package org.inca.diff.diffable

import org.inca.diff.diffable.DiffData.DiffableNoHoles

trait DiffableOracle {
  def predict(t: Diffable): Option[MetaVar]
}
trait MkDiffableOracle {
  // which common subtree
  // must be injective:
  // if apply(s, d).predict(x) ≡ apply(s, d).predict(y) ≡ Just v, then x ≡ y
  def apply(src: DiffableNoHoles, dest: DiffableNoHoles): DiffableOracle
}

