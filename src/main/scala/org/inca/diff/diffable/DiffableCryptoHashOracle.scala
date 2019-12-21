package org.inca.diff.diffable

import org.apache.commons.collections4.trie.PatriciaTrie
import org.inca.diff.HasCryptoHash

object DiffableCryptoHashOracle extends MkDiffableOracle {
  override def apply(src: Diffable[_], dest: Diffable[_]): DiffableOracle = {
    val srcTrie = new PatriciaTrie[MetaVar[_]]()
    val intersectTrie = new PatriciaTrie[MetaVar[_]]()

    var freshCount = 0

    src.foreach(new DiffableForeach {
      override def apply[T <: Diffable[_]](t: T): Unit = {
        val key = t.$hashString
        srcTrie.put(key, new MetaVar[T](freshCount))
        freshCount += 1
      }
    })

    dest.foreach(new DiffableForeach {
      override def apply[T <: Diffable[_]](t: T): Unit = {
        val key = t.$hashString
        val mv = srcTrie.get(key)
        if (mv != null)
          intersectTrie.put(key, mv)
      }
    })

    new DiffableOracle {
      override def predict[T <: Diffable[_]](t: T): Option[MetaVar[T]] =
        Option(intersectTrie.get(t.$hashString).asInstanceOf[MetaVar[T]])
    }
  }
}

