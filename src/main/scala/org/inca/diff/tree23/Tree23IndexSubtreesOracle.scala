package org.inca.diff.tree23

import Tree23Diff._

object Tree23IndexSubtreesOracle extends MkTree32Oracle {
    // proof of concept only, as this is very, very slow

    override def apply(src: Tree23, dest: Tree23): Tree23Oracle = {
      val trees1 = subtrees(src)
      val trees2 = subtrees(dest)
      val both = trees1.intersect(trees2)
      val commonTreeList = both.toList
      t => {
          val ix = commonTreeList.indexOf(t)
          if (ix >= 0) Some(new MetaVar(ix)) else None
      }
    }

    def subtrees(t: Tree23): Set[Tree23] = t match {
      case Leaf(s) => Set(t)
      case Node2(t1, t2) => subtrees(t1) ++ subtrees(t2) + t
      case Node3(t1, t2, t3) => subtrees(t1) ++ subtrees(t2) ++ subtrees(t3) + t
    }
  }
