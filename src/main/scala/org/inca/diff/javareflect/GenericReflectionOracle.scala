package org.inca.diff.javareflect

import GenericReflectionDiff._

trait GenericReflectionOracle {
  def predict(t: Tree): Option[MetaVar]
}
trait MkGenericReflectionOracle {
  // which common subtree
  // must be injective:
  // if apply(s, d).predict(x) ≡ apply(s, d).predict(y) ≡ Just v, then x ≡ y
  def apply(src: Tree, dest: Tree): GenericReflectionOracle
}

