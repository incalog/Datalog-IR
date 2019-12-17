package org.inca.diff.diffable

import org.apache.commons.collections4.trie.PatriciaTrie

class DiffableCryptoHashOracle[T <: Diffable[T]] extends MkDiffableOracle[T] {
  override def apply(src: T, dest: T): DiffableOracle[T] = {
    val srcTrie = new PatriciaTrie[MetaVar]()
    val intersectTrie = new PatriciaTrie[MetaVar]()

    var freshCount = 0
    def fillSrcTrie(t: T): Unit = {
      val key = t.$hashString
      srcTrie.put(key, new MetaVar(freshCount))
      freshCount += 1
    }
    src.visitDiffable(fillSrcTrie)

    def fillIntersectTrie(t: T): Unit = {
      val key = t.$hashString
      val mv = srcTrie.get(key)
      if (mv != null)
        intersectTrie.put(key, mv)
    }
    dest.visitDiffable(fillIntersectTrie)

    t => Option(intersectTrie.get(t.$hashString))
  }
}

