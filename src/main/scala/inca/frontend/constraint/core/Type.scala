package inca.frontend.constraint.core

import inca.compiler.source.SourceLocation
import inca.frontend.util.Resolvable
import inca.util.Scala
import truechange.{JavaLitType, LitType}

import scala.annotation.tailrec
import scala.meta.quasiquotes._

sealed trait Type extends SourceLocation {
  def prettyprint: String
  def javastring: String
  def asScala: meta.Type

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

  override def asScala: meta.Type = t"Any"
}
case object TNothing extends Type {
  override def prettyprint: String = "Nothing"
  override def javastring: String = "nothing"
  override def asScala: meta.Type = t"Nothing"
}

case class TLiteral(litType: LitType) extends Type {
  override def prettyprint: String = litType match {
    case JavaLitType(cl) if cl == classOf[java.lang.Integer] => "Int"
    case JavaLitType(cl) => cl.getSimpleName
    case _ => throw new UnsupportedOperationException
  }

  override def javastring: String = litType match {
    case JavaLitType(cl) => cl.getCanonicalName.replace(".", "$$")
    case _ => throw new UnsupportedOperationException
  }

  override def asScala: meta.Type = litType match {
    case JavaLitType(cl) =>  Scala.mkQualTypename(cl.getCanonicalName)
    case _ => throw new UnsupportedOperationException
  }
}
object TLiteral {
  val Bool: TLiteral = TLiteral(JavaLitType(classOf[java.lang.Boolean]))
  val Int: TLiteral = TLiteral(JavaLitType(classOf[java.lang.Integer]))
  val Long: TLiteral = TLiteral(JavaLitType(classOf[java.lang.Long]))
  val Double: TLiteral = TLiteral(JavaLitType(classOf[java.lang.Double]))
  val String: TLiteral = TLiteral(JavaLitType(classOf[java.lang.String]))
}

sealed trait TLinked extends Type {
  override def asScala: meta.Type = t"truechange.URI"
}
case object TAnyLinked extends TLinked {
  override def prettyprint: String = "AnyNode"
  override def javastring: String = "anynode"
}
case class TNode(name: String) extends TLinked  with Resolvable[TNode] {
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
  override def asScala: meta.Type = t"truechange.URI"
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

  override def asScala: meta.Type = t"(..${ts.map(_.asScala).toList})"
}

case class TScala(ty: Scala[meta.Type]) extends Type {
  override def prettyprint: String = s"`${ty.syntax}`"
  override def javastring: String = ???

  override def asScala: meta.Type = ty.tree
}
object TScala {
  def apply(typeString: String): TScala = {
    import meta.parsers._
    new TScala(Scala(typeString.parse[meta.Type].get))
  }
}
object TScalaBoolean extends TScala(Scala(t"Boolean"))
object TScalaInt extends TScala(Scala(t"Int"))
object TScalaLong extends TScala(Scala(t"Long"))
object TScalaDouble extends TScala(Scala(t"Double"))
object TScalaString extends TScala(Scala(t"String"))
object TScalaAny extends TScala(Scala(t"Any"))
