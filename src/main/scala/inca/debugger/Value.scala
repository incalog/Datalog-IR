package inca.debugger

import truechange.URI

sealed trait Value {
  def asURI: URI
  def asScala: Any
  def inner: Any
}
case class URIValue(uri: URI) extends Value {
  override def toString: String = uri.toString
  override def asURI: URI = uri
  override def asScala: Any = throw new IllegalArgumentException("Cannot convert uri value to scala value")
  override def inner: Any = uri
}
case class ScalaValue(v: Any) extends Value {
  override def toString: String = v.toString
  override def asURI: URI = throw new IllegalArgumentException("Cannot convert scala value to uri value")
  override def asScala: Any = v
  override def inner: Any = v
}
