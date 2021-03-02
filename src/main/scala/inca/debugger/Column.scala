package inca.debugger

import truechange.{Type, URI}

case class Column(name: String, value: ColumnValue, ty: ColumnType)


sealed trait ColumnType

case object Input extends ColumnType
case object Output extends ColumnType


sealed trait ColumnValue {
  def prettyPrint(): String
}

case class URIValue(uri: URI, types: Seq[Type]) extends ColumnValue {
  override def toString: String = prettyPrint()

  override def prettyPrint(): String = uri.toString
}

case class ScalaValue(v: Any) extends ColumnValue {
  override def toString: String = prettyPrint()

  override def prettyPrint(): String = "ScalaValue[" + v.toString + "]"
}
