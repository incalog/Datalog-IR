package inca.runtime.data

class DataURI(val repr: String) extends truechange.URI {
  override def toString: String = repr + "@" + Integer.toHexString(hashCode())
}
object DataURI {
  def apply(constr: String, args: Any*): DataURI = {
    val strArgs = args.map {
      case data: DataURI => data.repr
      case arg => arg.toString
    }
    new DataURI(constr + strArgs.mkString("(", ", ", ")"))
  }
}
