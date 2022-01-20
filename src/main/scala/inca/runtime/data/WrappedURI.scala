package inca.runtime.data

class WrappedURI(val uri: truechange.URI, obj: Any) extends truechange.URI {
  override def toString: String = obj.toString

  override def equals(obj: Any): Boolean = obj match {
    case other: WrappedURI => this.uri == other.uri
    case other: truechange.URI => this.uri == other
    case _ => false
  }

  override def hashCode(): Int = uri.hashCode()
}
