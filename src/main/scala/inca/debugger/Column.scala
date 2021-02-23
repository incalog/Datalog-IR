package inca.debugger

import truechange.{Type, URI}

case class Column(name: String, value: ColumnValue, ty: ColumnType)


sealed trait ColumnType

case object Input extends ColumnType
case object Output extends ColumnType


sealed trait ColumnValue {
  def prettyPrint(withTag: Boolean = true): String
}

case class ColURI(uri: URI, types: Seq[Type], tag: String) extends ColumnValue {
  override def toString: String = prettyPrint()

  override def prettyPrint(withTag: Boolean): String = uri.toString + (
    if (withTag && tag != null && tag.nonEmpty)
      ":" + tag
    else
      ""
    )
}

case class ColScalaType(v: Any) extends ColumnValue {
  override def toString: String = prettyPrint()

  override def prettyPrint(withTag: Boolean): String = v.toString
}
