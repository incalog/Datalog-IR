package inca.debugger

import truechange.URI

sealed trait Value {
  def asURI: URI
  def asScala: Any
  def unwrap: Any
}
object Value {
  def apply(v: Any): Value = v match {
    case uri: URI => URIValue(uri)
    case _ => ScalaValue(v)
  }

  implicit def valueOrdering: Ordering[Value] = (x: Value, y: Value) =>
    (x, y) match {
      case (ScalaValue(xv: Int), ScalaValue(yv: Int)) =>
        Ordering.Int.compare(xv, yv)
      case (ScalaValue(xv: Boolean), ScalaValue(yv: Boolean)) =>
        Ordering.Boolean.compare(xv, yv)
      case (ScalaValue(xv: Long), ScalaValue(yv: Long)) =>
        Ordering.Long.compare(xv, yv)
      case (ScalaValue(xv: Double), ScalaValue(yv: Double)) =>
        Ordering.Double.TotalOrdering.compare(xv, yv)
      case (ScalaValue(xv: String), ScalaValue(yv: String)) =>
        Ordering.String.compare(xv, yv)
      case (URIValue(xuri), URIValue(yuri)) =>
        if (xuri == yuri) 0
        else throw new IllegalArgumentException("HOW DO WE COMPARE URIS?")
      case _ => throw new IllegalArgumentException(s"Compare on $x, $y not supported yet")
    }
}
case class URIValue(uri: URI) extends Value {
  override def toString: String = uri.toString
  override def asURI: URI = uri
  override def asScala: Any = throw new IllegalArgumentException(
    "Cannot convert uri value to scala value"
  )
  override def unwrap: Any = uri
}
case class ScalaValue(v: Any) extends Value {
  override def toString: String = v.toString
  override def asURI: URI = throw new IllegalArgumentException(
    "Cannot convert scala value to uri value"
  )
  override def asScala: Any = v
  override def unwrap: Any = v
}
