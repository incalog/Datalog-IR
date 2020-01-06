package org.inca.diff

import org.apache.commons.collections4.trie.PatriciaTrie

class DiffableCryptoHashOracle(src: Diffable[_]) {
  var srcTrie: PatriciaTrie[MetaVar[_]] = null

  {
    srcTrie = new PatriciaTrie[MetaVar[_]]()
    var freshCount = 0
    src.foreach(new DiffableForeach {
      override def apply[T <: Diffable[_]](t: T): Unit = {
        val key = t.$hashString
        srcTrie.put(key, new MetaVar[T](freshCount, t))
        freshCount += 1
      }
    })
  }

  def mkNextOracle(dest: Diffable[_]): DiffableOracle = {
    val destTrie = new PatriciaTrie[MetaVar[_]]()
    val intersectTrie = new PatriciaTrie[MetaVar[_]]()

    var freshCount = 0
    dest.foreach(new DiffableForeach {
      override def apply[T <: Diffable[_]](t: T): Unit = {
        val key = t.$hashString
        destTrie.put(key, new MetaVar[T](freshCount, t))
        freshCount += 1
        val mv = srcTrie.get(key)
        if (mv != null)
          intersectTrie.put(key, mv)
      }
    })

    srcTrie = destTrie

    new DiffableOracle {
      override def predict[T <: Diffable[_]](t: T): Option[MetaVar[T]] =
        Option(intersectTrie.get(t.$hashString).asInstanceOf[MetaVar[T]])
    }
  }
}

