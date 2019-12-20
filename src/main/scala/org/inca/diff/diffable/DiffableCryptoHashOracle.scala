package org.inca.diff.diffable

import org.apache.commons.collections4.trie.PatriciaTrie
import org.inca.diff.HasCryptoHash

object DiffableCryptoHashOracle extends MkDiffableOracle {
  override def apply(src: Diffable[_], dest: Diffable[_]): DiffableOracle = {
    val srcTrie = new PatriciaTrie[MetaVar]()
    val intersectTrie = new PatriciaTrie[MetaVar]()

    var freshCount = 0
    def fillSrcTrie(t: HasCryptoHash): Unit = {
      val key = t.$hashString
      srcTrie.put(key, new MetaVar(freshCount))
      freshCount += 1
    }
    src.initOracle(fillSrcTrie)

    def fillIntersectTrie(t: HasCryptoHash): Unit = {
      val key = t.$hashString
      val mv = srcTrie.get(key)
      if (mv != null)
        intersectTrie.put(key, mv)
    }
    dest.initOracle(fillIntersectTrie)

    t => Option(intersectTrie.get(t.$hashString))
  }
}

