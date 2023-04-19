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
  override def toString: String = prettyprint
  def signature: String
}
case object TAny extends Type {
  override def prettyprint: String = "Any"
  override def flatten: Seq[Type] = Seq(this)
  override def asScala: meta.Type = t"Any"
  override def signature: String = "Any"
}
case object TNull extends Type {
  override def prettyprint: String = "Null"
  override def flatten: Seq[Type] = Seq(this)
  override def asScala: meta.Type = t"inca.runtime.data.ObjectID" //t"truechange.URI" // t"Null"
  override def signature: String = "Null"
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
  override def signature: String = ts.size match {
    case 0 => "Unit"
    case 1 => ts.head.signature
    case _ => ts.map(_.signature).mkString("Tuple_", "_", "")
  }
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
  override def signature: String = s"${ty.syntax}"
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
  override def prettyprint: String = ref.toString
  override def flatten: Seq[Type] = Seq(this)
  override def asScala: meta.Type = t"inca.runtime.data.ObjectID" //t"truechange.URI"
  var tyParams: Seq[Type] = Seq()
  override def signature: String = ref.name.raw
}

case class TSet(ty: Type) extends Type {
  override def prettyprint: String = s"Set[${ty.prettyprint}]"
  override def asScala: meta.Type = ty.asScala
  override def flatten: Seq[Type] = ty.flatten
  override def asSet: Option[TSet] = Some(this)
  override def signature: String = s"Set_${ty.signature}"
}
