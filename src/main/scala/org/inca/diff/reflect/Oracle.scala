package org.inca.diff.reflect

import Diff._

trait Oracle {
  def predict(t: Tree): Option[MetaVar]
}
trait MkOracle {
  // which common subtree
  // must be injective:
  // if apply(s, d).predict(x) ≡ apply(s, d).predict(y) ≡ Just v, then x ≡ y
  def apply(src: Tree, dest: Tree): Oracle
}

