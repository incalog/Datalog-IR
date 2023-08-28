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

  var tyArgs: Seq[Type] = Seq()
}

object Type {
  def suffix(typ: Type): String = typ match {
    case TAny => "Any"
    case TNull => "Null"
    case TTuple(ts) => "Tuple_" + ts.map(suffix).mkString("_")
    case TScala(ty) => ty.syntax
    case TClass(TName(Name(raw))) => raw
    case TSet(ty) => "Set_" + suffix(ty)
    case TName(n) => n.raw
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

case class TClass(ref: TName) extends Type {

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



case class TName(name: Name) extends Type with Resolvable[TName.Target] {
  override def prettyprint: Signature = name.toString
  override def flatten: Seq[Type] = Seq()
  override def asScala: meta.Type = t"Any"

  def classDef: Option[ClassDef] = this.target match {
      case Some(classDef: ClassDef) => Some(classDef)
      case None => None
  }

  def genericParamDef: Option[GenericParamDef] = this.target match {
    case Some(genericParamDef: GenericParamDef) => Some(genericParamDef)
    case None => None
  }
}

object TName {
  trait Target extends SourceLocation
}