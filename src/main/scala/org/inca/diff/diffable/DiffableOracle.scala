package org.inca.diff.diffable

trait DiffableOracle[T] {
  def predict(t: T): Option[MetaVar]
}
trait MkDiffableOracle[T] {
  // which common subtree
  // must be injective:
  // if apply(s, d).predict(x) ≡ apply(s, d).predict(y) ≡ Just v, then x ≡ y
  def apply(src: T, dest: T): DiffableOracle[T]
}

