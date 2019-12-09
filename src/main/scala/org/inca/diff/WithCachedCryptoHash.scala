package org.inca.diff

import java.util.Base64

trait WithCachedCryptoHash {
  val $hash: Array[Byte]
  lazy val $hashString = Base64.getEncoder.encodeToString($hash)
}