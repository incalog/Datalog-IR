package org.inca.diff.reflect

import GenericReflection._

object GenericReflectionIndexSubtreesOracle extends MkGenericReflectionOracle {
  // proof of concept only, as this is very, very slow

  override def apply(src: Node, dest: Node): GenericReflectionOracle = {
    val trees1 = subtrees(src)
    val trees2 = subtrees(dest)
    val both = trees1.intersect(trees2)
    val commonTreeList = both.toList
    t => {
        val ix = commonTreeList.indexOf(t)
        if (ix >= 0) Some(new MetaVar(ix)) else None
    }
  }

  def subtrees(node: Node): Set[Node] = {
    if (node == null)
      return Set()
    val cls = node.getClass
    if (cls.extendsStructuralDiff) {
      val vals = cls.allFieldVals(node)
      val subs = vals.foldLeft(Set[Node]())(_ union subtrees(_))
      subs + node
    }
    else
      Set(node)
  }
}
