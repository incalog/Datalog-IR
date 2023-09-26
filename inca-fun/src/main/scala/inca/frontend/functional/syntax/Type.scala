package inca.frontend.functional.syntax

import inca.Scala
import inca.ir.typing.Resolvable
import inca.ir.util.SourceLocation

sealed trait Type extends SourceLocation {
  def prettyprint: String
  def flatten: Seq[Type]
  def freeTvars: Seq[TName]
  override def toString: String = prettyprint
}
case object TAny extends Type {
  override def prettyprint: String = "Any"
  override def flatten: Seq[Type] = Seq(this)
  override def freeTvars: Seq[TName] = Seq()
}
case object TNothing extends Type {
  override def prettyprint: String = "Nothing"
  override def flatten: Seq[Type] = Seq(this)
  override def freeTvars: Seq[TName] = Seq()
}

case class TFun(from: Seq[Type], to: Type) extends Type {
  override def prettyprint: String = from.size match {
    case 0 => s"() => ${to.prettyprint}"
    case 1 => s"(${from.head.prettyprint} => ${to.prettyprint})"
    case _ => s"(${from.map(_.prettyprint).mkString("(", ", ", ")")} => ${to.prettyprint})"
  }

  override def flatten: Seq[Type] = Seq(this)
  override def freeTvars: Seq[TName] = to.freeTvars ++ from.flatMap(_.freeTvars)
}

val TUnit: TTuple = TTuple(Seq.empty)

case class TTuple(ts: Seq[Type]) extends Type {
  if (ts.size == 1)
    throw new IllegalArgumentException(s"Avoid creating 1-ary tuples.")
  override def prettyprint: String = ts.size match {
    case 0 => "Unit"
    case 1 => ts.head.prettyprint
    case _ => ts.map(_.prettyprint).mkString("(", ", ", ")")
  }
  override def flatten: Seq[Type] = ts.flatMap(_.flatten)
  override def freeTvars: Seq[TName] = ts.flatMap(_.freeTvars)
}
object TTuple {
  def from(ts: Seq[Type]): Type = ts match {
    case Nil => TUnit
    case t :: Nil => t
    case ts => TTuple(ts)
  }
}
case class TName(name: Name) extends Type with Resolvable[TName.Target] {
  override def prettyprint: String = name.name
  override def flatten: Seq[Type] = Seq(this)
  override def freeTvars: Seq[TName] = Seq(this)
}

object TName {
  trait Target
}

case class TConstr(name: Name, tys: Seq[Type]) extends Type with Resolvable[TName.Target] {
  override def prettyprint: String =
    s"${name.name}" + (if(tys.isEmpty) "" else s"[${tys.map(_.prettyprint).mkString(", ")}]")
  override def flatten: Seq[Type] = Seq(this)
  // TODO fix?
  override def freeTvars: Seq[TName] = tys.flatMap(_.freeTvars)
}

case class TScala(ty: Scala.Type) extends Type {
  override def prettyprint: String = s"`$ty`"
  override def flatten: Seq[Type] = Seq(this)
  override def freeTvars: Seq[TName] = Seq()
}
object TScalaBoolean extends TScala(Scala.TypeName("Boolean"))
object TScalaInt extends TScala(Scala.TypeName("Int"))
object TScalaLong extends TScala(Scala.TypeName("Long"))
object TScalaDouble extends TScala(Scala.TypeName("Double"))
object TScalaString extends TScala(Scala.TypeName("String"))
object TScalaAny extends TScala(Scala.TypeName("Any"))


case class TOption(ty: Type) extends Type {
  override def prettyprint: String = s"Option[${ty.prettyprint}]"
  override def flatten: Seq[Type] = ty.flatten
  override def freeTvars: Seq[TName] = ty.freeTvars
}

case class TSet(ty: Type) extends Type {
  override def prettyprint: String = s"Set[${ty.prettyprint}]"
  override def flatten: Seq[Type] = ty.flatten
  override def freeTvars: Seq[TName] = ty.freeTvars
}
