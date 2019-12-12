package org.inca.diff.tree23

import org.inca.diff.tree23.Tree23Diff._

trait Tree23Oracle {
  def predict(t: Tree23): Option[MetaVar]
}
trait MkTree32Oracle {
  // which common subtree
  // must be injective:
  // if apply(s, d).predict(x) ≡ apply(s, d).predict(y) ≡ Just v, then x ≡ y
  def apply(src: Tree23, dest: Tree23): Tree23Oracle
}

