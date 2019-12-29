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
      case v: Short  => d.update(intToBytes(v))
      case v: Char  => d.update(intToBytes(v))
      case v: Int => d.update(intToBytes(v))
      case v: Long => d.update(longToBytes(v))
      case v: Float => d.update(floatToBytes(v))
      case v: Double => d.update(doubleToBytes(v))
      case v: String => d.update(v.getBytes)
      case v: Symbol => d.update(v.name.getBytes)
      case _ => throw new IllegalArgumentException(s"Cannot compute hash of $v")
    }
  }

  private def intToBytes(data: Int) = Array[Byte](
    ((data >> 24) & 0xff).toByte,
    ((data >> 16) & 0xff).toByte,
    ((data >> 8) & 0xff).toByte,
    ((data >> 0) & 0xff).toByte)

  private def longToBytes(data: Long) = Array[Byte](
    ((data >> 56) & 0xff).toByte,
    ((data >> 48) & 0xff).toByte,
    ((data >> 40) & 0xff).toByte,
    ((data >> 32) & 0xff).toByte,
    ((data >> 24) & 0xff).toByte,
    ((data >> 16) & 0xff).toByte,
    ((data >> 8) & 0xff).toByte,
    ((data >> 0) & 0xff).toByte)

  private def floatToBytes(data: Float) = {
    import java.nio.ByteBuffer
    val bytes = new Array[Byte](4)
    ByteBuffer.wrap(bytes).putFloat(data)
    bytes
  }

  private def doubleToBytes(data: Double) = {
    import java.nio.ByteBuffer
    val bytes = new Array[Byte](8)
    ByteBuffer.wrap(bytes).putDouble(data)
    bytes
  }

  def $hash: Array[Byte]
  lazy val $hashString = Base64.getEncoder.encodeToString($hash)
}