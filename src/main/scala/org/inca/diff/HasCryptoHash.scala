package org.inca.diff

import java.security.MessageDigest
import java.util.Base64

trait HasCryptoHash {
  final def mkDigest: MessageDigest = MessageDigest.getInstance("SHA-256")

  final def hashNonDiffable(v: Any, d: MessageDigest): Unit = {
    d.update(v.getClass.getCanonicalName.getBytes())
    v match {
      case v: Boolean => d.update(if(v) 1:Byte else 0:Byte)
      case v: Byte => d.update(v)
      case v: Short  => d.update(BigInt(v).toByteArray)
      case v: Char  => d.update(BigInt(v).toByteArray)
      case v: Int => d.update(BigInt(v).toByteArray)
      case v: Long => d.update(BigInt(v).toByteArray)
      case v: Float => d.update(BigInt(java.lang.Float.floatToRawIntBits(v)).toByteArray)
      case v: Double => d.update(BigInt(java.lang.Double.doubleToRawLongBits(v)).toByteArray)
      case v: String => d.update(v.getBytes)
      case v: Symbol => d.update(v.name.getBytes)
      case _ => throw new IllegalArgumentException(s"Cannot compute hash of $v")
    }
  }

  def $hash: Array[Byte]
  lazy val $hashString = Base64.getEncoder.encodeToString($hash)
}