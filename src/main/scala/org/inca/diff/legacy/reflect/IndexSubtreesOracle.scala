package org.inca.diff.legacy.reflect

import Diff._

object IndexSubtreesOracle extends MkOracle {
  // proof of concept only, as this is very, very slow

  override def apply(src: Tree, dest: Tree): Oracle = {
    val trees1 = subtrees(src)
    val trees2 = subtrees(dest)
    val both = trees1.intersect(trees2)
    val commonTreeList = both.toList
    t => {
        val ix = commonTreeList.indexOf(t)
        if (ix >= 0) Some(new MetaVar(ix)) else None
    }
  }

  def subtrees(t: Tree): Set[Tree] = t match {
    case Val(_) => Set(t)
    case Node(cls, subs) => subs.foldLeft(Set[Tree]())(_ union subtrees(_)) + t
  }
}
