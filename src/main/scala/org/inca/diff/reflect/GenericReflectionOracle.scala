package org.inca.diff.reflect

import GenericReflection._

trait GenericReflectionOracle {
  def predict(t: Node): Option[MetaVar]
}
trait MkGenericReflectionOracle {
  // which common subtree
  // must be injective:
  // if apply(s, d).predict(x) ≡ apply(s, d).predict(y) ≡ Just v, then x ≡ y
  def apply(src: Node, dest: Node): GenericReflectionOracle
}

