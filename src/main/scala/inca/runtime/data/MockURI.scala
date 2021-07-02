package inca.runtime.data

class MockURI(val repr: String) extends truechange.URI {
  override def equals(obj: Any): Boolean = obj match {
    case other: MockURI => this.repr == other.repr
    case _ => false
  }

  override def hashCode(): Int = repr.hashCode

  override def toString: String = repr
}

object MockURI {
  val DEBUG_PRINT = true

  def apply(constr: String, args: Any*): MockURI = {
    val strArgs = args.map {
      case data: MockURI => data.repr
      case arg => arg.toString
    }
    new MockURI(constr + strArgs.mkString("(", ", ", ")"))
  }
}
