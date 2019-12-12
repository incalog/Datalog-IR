package org.inca.diff.diffable

import org.apache.commons.collections4.trie.PatriciaTrie
import org.inca.diff.diffable.DiffData.DiffableNoHoles

object DiffableCryptoHashOracle extends MkDiffableOracle {
  override def apply(src: DiffableNoHoles, dest: DiffableNoHoles): DiffableOracle = {
    val srcTrie = new PatriciaTrie[MetaVar]()
    val intersectTrie = new PatriciaTrie[MetaVar]()

    var freshCount = 0
    def fillSrcTrie(t: Diffable): Unit = {
      val key = t.$hashString
      srcTrie.put(key, new MetaVar(freshCount))
      freshCount += 1
    }
    src.visitDiffable(fillSrcTrie)

    def fillIntersectTrie(t: Diffable): Unit = {
      val key = t.$hashString
      val mv = srcTrie.get(key)
      if (mv != null)
        intersectTrie.put(key, mv)
    }
    dest.visitDiffable(fillIntersectTrie)

    t => Option(intersectTrie.get(t.$hashString))
  }
}

