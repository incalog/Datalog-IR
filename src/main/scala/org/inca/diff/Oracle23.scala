package org.inca.diff

import org.inca.diff.Tree23._

trait Oracle23 {
  def predict(t: Tree23): Option[MetaVar]
}
trait MkOracle23 {
  // which common subtree
  // must be injective:
  // if apply(s, d).predict(x) ≡ apply(s, d).predict(y) ≡ Just v, then x ≡ y
  def apply(src: Tree23, dest: Tree23): Oracle23
}

