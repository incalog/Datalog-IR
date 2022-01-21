package inca.frontend.functional.core

import inca.compiler.source.SourceLocation
import inca.frontend.util.Resolvable
import inca.util.Scala

import scala.meta.quasiquotes._

sealed trait Type extends SourceLocation {
  def prettyprint: String
  def asScala: meta.Type
  def flatten: Seq[Type]
  def freeTvars: Seq[TData]
  override def toString: String = prettyprint
}
case object TAny extends Type {
  override def prettyprint: String = "Any"
  override def asScala: meta.Type = t"Any"
  override def flatten: Seq[Type] = Seq(this)
  override def freeTvars: Seq[TData] = Seq()
}
case object TNothing extends Type {
  override def prettyprint: String = "Nothing"
  override def asScala: meta.Type = t"Nothing"
  override def flatten: Seq[Type] = Seq(this)
  override def freeTvars: Seq[TData] = Seq()
}

case class TFun(from: Seq[Type], to: Type) extends Type {
  override def prettyprint: String = from.size match {
    case 0 => s"() => ${to.prettyprint}"
    case 1 => s"(${from.head.prettyprint} => ${to.prettyprint})"
    case _ => s"(${from.map(_.prettyprint).mkString("(", ", ", ")")} => ${to.prettyprint})"
  }

  override def asScala: meta.Type = meta.Type.Function(from.map(_.asScala).toList, to.asScala)

  override def flatten: Seq[Type] = Seq(this)
  override def freeTvars: Seq[TData] = to.freeTvars ++ from.flatMap(_.freeTvars)
}

case class TTuple(ts: Seq[Type]) extends Type {
  if (ts.size == 1)
    throw new IllegalArgumentException(s"Avoid creating 1-ary tuples.")
  override def prettyprint: String = ts.size match {
    case 0 => "Unit"
    case 1 => ts.head.prettyprint
    case _ => ts.map(_.prettyprint).mkString("(", ", ", ")")
  }
  override def asScala: meta.Type = t"(..${ts.map(_.asScala).toList})"
  override def flatten: Seq[Type] = ts.flatMap(_.flatten)
  override def freeTvars: Seq[TData] = ts.flatMap(_.freeTvars)
}
object TTuple {
  def from(ts: Seq[Type]): Type = ts match {
    case Nil => TUnit
    case t :: Nil => t
    case ts => TTuple(ts)
  }
}

case class TData(name: Name) extends Type with Resolvable[TData.Target] {
  override def prettyprint: String = name.name
  override def asScala: meta.Type = t"truechange.URI"
  override def flatten: Seq[Type] = Seq(this)
  override def freeTvars: Seq[TData] = Seq(this)
}
object TData {
  trait Target
}

case class TScala(ty: Scala[meta.Type]) extends Type {
  override def prettyprint: String = s"`${ty.syntax}`"
  override def asScala: meta.Type = ty.tree
  override def flatten: Seq[Type] = Seq(this)
  override def freeTvars: Seq[TData] = Seq()
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


case class TOption(ty: Type) extends Type {
  override def prettyprint: String = s"Option[${ty.prettyprint}]"
  override def asScala: meta.Type = ty.asScala
  override def flatten: Seq[Type] = ty.flatten
  override def freeTvars: Seq[TData] = ty.freeTvars
}

case class TSet(ty: Type) extends Type {
  override def prettyprint: String = s"Set[${ty.prettyprint}]"
  override def asScala: meta.Type = ty.asScala
  override def flatten: Seq[Type] = ty.flatten
  override def freeTvars: Seq[TData] = ty.freeTvars
}
