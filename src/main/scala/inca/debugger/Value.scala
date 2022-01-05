package inca.debugger

import truechange.URI

sealed trait Value {
  def prettyPrint: String
}
case class URIValue(uri: URI) extends Value {
  override def toString: String = prettyPrint
  override def prettyPrint: String = uri.toString
}
case class ScalaValue(v: Any) extends Value {
  override def toString: String = prettyPrint
  override def prettyPrint: String = v.toString
}
