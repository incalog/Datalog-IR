package inca.runtime.data

import truediff.Diffable

class DataURI(val repr: String) extends truechange.URI {
  override def toString: String =
    if (DataURI.DEBUG_PRINT)
      repr.substring(0, Math.min(20, repr.length)) + "@" + Integer.toHexString(hashCode())
    else
      "@" + Integer.toHexString(hashCode())

  def ~=(uri: truechange.URI): Boolean = uri match {
    case other: DataURI => this.repr == other.repr
    case _ => false
  }
}
object DataURI {
  val DEBUG_PRINT = true

  def apply(constr: String, args: Any*): DataURI = {
    val strArgs = args.map {
      case data: DataURI => data.repr
      case diff: Diffable if diff.uri.isInstanceOf[DataURI] => diff.uri.asInstanceOf[DataURI].repr
      case arg => arg.toString
    }
    new DataURI(constr + strArgs.mkString("(", ", ", ")"))
  }
}
