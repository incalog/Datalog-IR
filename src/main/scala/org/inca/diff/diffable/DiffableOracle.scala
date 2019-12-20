package org.inca.diff.diffable

import org.inca.diff.HasCryptoHash

trait DiffableOracle {
  def predict(t: HasCryptoHash): Option[MetaVar]
}
trait MkDiffableOracle {
  // which common subtree
  // must be injective:
  // if apply(s, d).predict(x) ≡ apply(s, d).predict(y) ≡ Just v, then x ≡ y
  def apply(src: Diffable[_], dest: Diffable[_]): DiffableOracle
}

