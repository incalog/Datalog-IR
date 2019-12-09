package org.inca.diff.reflect

import java.security.MessageDigest
import java.util.Base64

import org.apache.commons.collections4.trie.PatriciaTrie
import org.inca.diff.WithCachedCryptoHash
import org.inca.diff.reflect.GenericReflection._

object GenericReflectionCryptoHashOracle extends MkGenericReflectionOracle {
  val digest: MessageDigest = MessageDigest.getInstance("SHA-256")

  override def apply(src: Node, dest: Node): GenericReflectionOracle = {
    val srcTrie = new PatriciaTrie[MetaVar]()
    val intersectTrie = new PatriciaTrie[MetaVar]()

    var freshCount = 0
    def fillSrcTrie(t: Node): Unit = {
      if (!t.isInstanceOf[StructuralDiff])
        return
      val key = hashString(t)
      srcTrie.put(key, new MetaVar(freshCount))
      freshCount += 1
      t.getClass.allFieldVals(t).foreach(fillSrcTrie(_))
    }
    fillSrcTrie(src)

    def fillIntersectTrie(t: Node): Unit = {
      if (!t.isInstanceOf[StructuralDiff])
        return
      val key = hashString(t)
      val mv = srcTrie.get(key)
      if (mv != null)
        intersectTrie.put(key, mv)
      t.getClass.allFieldVals(t).foreach(fillIntersectTrie(_))
    }
    fillIntersectTrie(dest)

    t => {
      if (t.isInstanceOf[StructuralDiff])
        Option(intersectTrie.get(hashString(t)))
      else
        None
    }
  }

  def hashString(t: Node): String = t match {
      case t: WithCachedCryptoHash => t.$hashString
      case _ => Base64.getEncoder.encodeToString(computeHash(t))
    }

  def computeHash(t: Node): Array[Byte] = {
    val cls = t.getClass
    digest.update(cls.getCanonicalName.getBytes())
    cls.allFieldVals(t).foreach (v => digest.update(computeHash(v)))
    digest.digest()
  }
}

