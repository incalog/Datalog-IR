package inca.frontend.core

import inca.compiler.SourceLocation
import inca.frontend.typechecker.Resolvable
import inca.util.Meta.Scala

import scala.meta.quasiquotes._

sealed trait Type extends SourceLocation {
  def prettyprint: String
  def javastring: String
  def asScala: meta.Type

  override def toString: String = prettyprint
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

case class TData(name: Name) extends Type with Resolvable[TData.Target] {
  override def prettyprint: String = name.name
  override def javastring: String = name.name
  override def asScala: meta.Type = t"String"
}
object TData {
  trait Target
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
