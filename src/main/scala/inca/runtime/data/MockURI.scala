package inca.runtime.data

import truediff.Diffable

class MockURI(repr: String) extends DataURI(repr) {
  override def equals(obj: Any): Boolean = obj match {
    case other: MockURI => this.repr == other.repr
    case _ => false
  }

  override def hashCode(): Int = repr.hashCode
}

object MockURI {
  val DEBUG_PRINT = true

  def apply(constr: String, args: Any*): MockURI = {
    val strArgs = args.map {
      case data: MockURI => data.repr
      case diff: Diffable if diff.uri.isInstanceOf[DataURI] => diff.uri.asInstanceOf[DataURI].repr
      case arg => arg.toString
    }
    new MockURI(constr + strArgs.mkString("(", ", ", ")"))
  }
}
