package inca.runtime.data

class JVMURI_Repr(val repr: String) extends truechange.URI {
  override def toString: String = repr + "@" + Integer.toHexString(hashCode())
}
