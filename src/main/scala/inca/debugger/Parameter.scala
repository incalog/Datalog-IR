package inca.debugger

import truechange.{Type, URI}

case class Parameter(name: String, value: ParameterValue, io: ParameterIO)


sealed trait ParameterIO

case object InputParam extends ParameterIO
case object OutputParam extends ParameterIO


sealed trait ParameterValue {
  def toString(withTag: Boolean): String
}

case class PURI(uri: URI, types: Seq[Type], tag: String) extends ParameterValue {
  override def toString: String = toString(true)
  override def toString(withTag: Boolean): String = if(withTag) uri + ":" + tag else uri.toString
}

case class PScalaType(v: Any) extends ParameterValue {
  override def toString: String = toString(true)
  override def toString(withTag: Boolean): String = v.toString
}
