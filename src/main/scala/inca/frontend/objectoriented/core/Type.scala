package inca.frontend.objectoriented.core

import inca.compiler.SourceLocation
import inca.frontend.util.Resolvable
import inca.util.Scala

import scala.meta.XtensionQuasiquoteType

sealed trait Type extends SourceLocation {
  def prettyprint: String
  def flatten: Seq[Type]
  def asScala: meta.Type
  override def toString: String = prettyprint
  var innerType: Option[Type] = None
}
case object TAny extends Type {
  override def prettyprint: String = "Any"
  override def flatten: Seq[Type] = Seq(this)
  override def asScala: meta.Type = t"Any"
}
case object TNull extends Type {
  override def prettyprint: String = "Null"
  override def flatten: Seq[Type] = Seq(this)
  override def asScala: meta.Type = t"truechange.URI"
}

// TODO: We do not need this right now
/*case object TNothing extends Type {
  override def prettyprint: String = "Nothing"
  override def flatten: Seq[Type] = Seq(this)
}*/

case class TTuple(ts: Seq[Type]) extends Type {
  if (ts.size == 1)
    throw new IllegalArgumentException(s"Avoid creating 1-ary tuples.")
  override def prettyprint: String = ts.size match {
    case 0 => "Unit"
    case 1 => ts.head.prettyprint
    case _ => ts.map(_.prettyprint).mkString("(", ", ", ")")
  }
  override def flatten: Seq[Type] = ts.flatMap(_.flatten)
  override def asScala: meta.Type = t"(..${ts.map(_.asScala).toList})"

  innerType = Some(TAny)
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
  override def prettyprint: String = ref.toString
  override def flatten: Seq[Type] = Seq(this)
  override def asScala: meta.Type = t"truechange.URI"
}

case class TSet(ty: Type) extends Type {
  override def prettyprint: String = s"Set[${ty.prettyprint}]"
  override def asScala: meta.Type = ty.asScala
  override def flatten: Seq[Type] = ty.flatten

  innerType = Some(ty)
}
