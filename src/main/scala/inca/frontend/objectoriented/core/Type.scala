package inca.frontend.objectoriented.core

import inca.compiler.SourceLocation
import inca.frontend.util.Resolvable
import inca.util.Scala

import scala.meta.XtensionQuasiquoteType

sealed trait Type extends SourceLocation {
  def prettyprint: String
  def flatten: Seq[Type]
  def asScala: meta.Type
  def asSet: Option[TSet] = None
  def isUnit: Boolean = false
  override def toString: String = prettyprint
}

object Type {
  def suffix(typ: Type): String = typ match {
    case TAny => "Any"
    case TNull => "Null"
    case TTuple(ts) => "Tuple_" + ts.map(suffix).mkString("_")
    case TScala(ty) => ty.syntax
    case TClass(ClassRef(Name(raw),genericType)) => raw
    case TSet(ty) => "Set_" + suffix(ty)
  }
}

case object TAny extends Type {
  override def prettyprint: String = "Any"
  override def flatten: Seq[Type] = Seq(this)
  override def asScala: meta.Type = t"Any"
}
case object TNull extends Type {
  override def prettyprint: String = "Null"
  override def flatten: Seq[Type] = Seq(this)
  override def asScala: meta.Type = t"inca.runtime.data.objectoriented.Identity" //t"truechange.URI" // t"Null"
}

case class TTuple(ts: Seq[Type]) extends Type {
  if (ts.size == 1)
    throw new IllegalArgumentException(s"Avoid creating 1-ary tuples.")
  override def prettyprint: String = ts.size match {
    case 0 => "Unit"
    case 1 => ts.head.prettyprint
    case _ => ts.map(_.prettyprint).mkString("(", ", ", ")")
  }
  override def flatten: Seq[Type] = ts.flatMap(_.flatten)
  override def asScala: meta.Type = {
    if (ts.isEmpty)
      t"Unit"
    else
      t"(..${ts.map(_.asScala).toList})"
  }
  override def isUnit: Boolean = ts.isEmpty
}
object TTuple {
  def from(ts: Seq[Type]): Type = ts match {
    case Nil => TUnit
    case t :: Nil => t
    case ts => TTuple(ts)
  }
}

case class TScala(ty: Scala[meta.Type]) extends Type {
  override def prettyprint: String = s"`${ty.syntax}`"
  override def flatten: Seq[Type] = Seq(this)
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

case class TClass(ref: ClassRef) extends Type {
  println("TClass")
  println(s"### ${ref.name} ${ref.typesForTypeparameters}")

  override def prettyprint: String = ref.toString
  override def flatten: Seq[Type] = Seq(this)
  override def asScala: meta.Type = t"inca.runtime.data.objectoriented.Identity" //t"truechange.URI"
  var tyParams: Seq[Type] = Seq()
}

case class TSet(ty: Type) extends Type {
  override def prettyprint: String = s"Set[${ty.prettyprint}]"
  override def asScala: meta.Type = ty.asScala
  override def flatten: Seq[Type] = ty.flatten
  override def asSet: Option[TSet] = Some(this)
}


// TODO
case class TGeneric(ty: Type, genericTyParam: Seq[Type]) extends Type {
  println("TGeneric")
  println(s"### $ty")
  println(s"### $genericTyParam")

  override def prettyprint: Signature = s"${ty.prettyprint}[${genericTyParam.map(t => t.prettyprint)}]"

  override def flatten: Seq[Type] = ???

  override def asScala: meta.Type = ???
}