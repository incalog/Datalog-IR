package inca.frontend.core

import inca.frontend.parser.SourceLocation
import inca.util.Meta.Scala

import scala.annotation.tailrec

sealed trait Type extends SourceLocation {
  def prettyprint: String
  def javastring: String

  override def toString: String = prettyprint

  @tailrec
  final def unroll: Type = this match {
    case ty: TEnumeration => ty.contained.unroll
    case ty => ty
  }
}
case object TAny extends Type {
  override def prettyprint: String = "Any"
  override def javastring: String = "any"
}
case object TNothing extends Type {
  override def prettyprint: String = "Nothing"
  override def javastring: String = "nothing"
}
case object TBool extends Type {
  override def prettyprint: String = "Boolean"
  override def javastring: String = "bool"
}
case object TInt extends Type {
  override def prettyprint: String = "Int"
  override def javastring: String = "int"
}
case object TLong extends Type {
  override def prettyprint: String = "Long"
  override def javastring: String = "long"
}
case object TDouble extends Type {
  override def prettyprint: String = "Double"
  override def javastring: String = "double"
}
case object TString extends Type {
  override def prettyprint: String = "String"
  override def javastring: String = "string"
}

trait TLinked extends Type
case object TAnyLinked extends TLinked {
  override def prettyprint: String = "Node"
  override def javastring: String = "node"
}
case class TNode(name: String) extends TLinked {
  override def prettyprint: String = name
  override def javastring: String = name.replace('.','_')
  def apply(field: String): NamedLink = NamedLink(Name(field))
}

sealed trait TIterable extends Type {
  val contained: Type
}
case class TList(contained: TLinked) extends TLinked with TIterable {
  override def prettyprint: String = s"List[${contained.prettyprint}]"
  override def javastring: String = s"List_${contained.javastring}"
}
case class TEnumeration(contained: Type) extends TIterable {
  override def prettyprint: String = s"Enum[${contained.prettyprint}]"
  override def javastring: String = s"Enum_${contained.javastring}"
}

case class TTuple(ts: Seq[Type]) extends Type {
  if (ts.size == 1)
    throw new IllegalArgumentException(s"Avoid creating 1-ary tuples.")

  override def prettyprint: String = ts.size match {
    case 0 => "Unit"
    case 1 => ts.head.prettyprint
    case _ => ts.map(_.prettyprint).mkString("(", ", ", ")")
  }

  override def javastring: String = "Tuple_" + ts.map(_.javastring).mkString("_")
}

case class ScalaType(ty: Scala[meta.Type]) extends Type {
  override def prettyprint: String = ty.syntax
  override def javastring: String = ???
}