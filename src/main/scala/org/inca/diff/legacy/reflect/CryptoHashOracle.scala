package org.inca.diff.legacy.reflect

import org.apache.commons.collections4.trie.PatriciaTrie
import org.inca.diff.legacy.reflect.Diff._

object CryptoHashOracle extends MkOracle {
  override def apply(src: Tree, dest: Tree): Oracle = {
    val srcTrie = new PatriciaTrie[MetaVar]()
    val intersectTrie = new PatriciaTrie[MetaVar]()

    var freshCount = 0
    def fillSrcTrie(t: Tree): Unit = {
      srcTrie.put(t.$hashString, new MetaVar(freshCount))
      freshCount += 1
      t match {
        case Node(_, subs) => subs.foreach(fillSrcTrie)
        case _ =>
      }
    }
    fillSrcTrie(src)

    def fillIntersectTrie(t: Tree): Unit = {
      val mv = srcTrie.get(t.$hashString)
      if (mv != null)
        intersectTrie.put(t.$hashString, mv)
      t match {
        case Node(_, subs) => subs.foreach(fillIntersectTrie)
        case _ =>
      }
    }
    fillIntersectTrie(dest)

    t => Option(intersectTrie.get(t.$hashString))
  }
}

