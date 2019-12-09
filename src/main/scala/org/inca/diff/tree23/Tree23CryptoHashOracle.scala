package org.inca.diff.tree23

import java.security.MessageDigest

import org.apache.commons.collections4.trie.PatriciaTrie
import org.inca.diff.tree23.Tree23._

object Tree23CryptoHashOracle extends MkTree32Oracle {
  val digest: MessageDigest = MessageDigest.getInstance("SHA-256")

  override def apply(src: Tree23, dest: Tree23): Tree23Oracle = {
    val srcTrie = new PatriciaTrie[MetaVar]()
    val intersectTrie = new PatriciaTrie[MetaVar]()

    var freshCount = 0
    def fillSrcTrie(t: Tree23): Unit = {
      srcTrie.put(t.$hashString, new MetaVar(freshCount))
      freshCount += 1
      t match {
        case Leaf(_) =>
        case Node2(t1, t2) => fillSrcTrie(t1); fillSrcTrie(t2)
        case Node3(t1, t2, t3) => fillSrcTrie(t1); fillSrcTrie(t2); fillSrcTrie(t3)
      }
    }
    fillSrcTrie(src)

    def fillIntersectTrie(t: Tree23): Unit = {
      val key = t.$hashString
      val mv = srcTrie.get(key)
      if (mv != null)
        intersectTrie.put(key, mv)
      t match {
        case Leaf(s) =>
        case Node2(t1, t2) => fillIntersectTrie(t1); fillIntersectTrie(t2)
        case Node3(t1, t2, t3) => fillIntersectTrie(t1); fillIntersectTrie(t2); fillIntersectTrie(t3)
      }
    }
    fillIntersectTrie(dest)

    t => Option(intersectTrie.get(t.$hashString))
  }
}

