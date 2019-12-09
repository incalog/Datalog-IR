package org.inca.diff.reflect

import java.security.MessageDigest
import java.util.Base64

import org.apache.commons.collections4.trie.PatriciaTrie
import org.inca.diff.WithCachedCryptoHash
import org.inca.diff.reflect.GenericReflectionDiff._

object GenericReflectionCryptoHashOracle extends MkGenericReflectionOracle {
  val digest: MessageDigest = MessageDigest.getInstance("SHA-256")

  override def apply(src: Tree, dest: Tree): GenericReflectionOracle = {
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

