package inca.frontend.objectoriented.core

import inca.compiler.SourceLocation
import inca.frontend.util.Resolvable

import scala.meta.quasiquotes._

sealed trait Type extends SourceLocation {
  def prettyprint: String
  def flatten: Seq[Type]
  override def toString: String = prettyprint
}
case object TAny extends Type {
  override def prettyprint: String = "Any"
  override def flatten: Seq[Type] = Seq(this)
}
case object TNothing extends Type {
  override def prettyprint: String = "Nothing"
  override def flatten: Seq[Type] = Seq(this)
}

case class TFun(from: Seq[Type], to: Type) extends Type {
  override def prettyprint: String = from.size match {
    case 0 => s"() => ${to.prettyprint}"
    case 1 => s"(${from.head.prettyprint} => ${to.prettyprint})"
    case _ => s"(${from.map(_.prettyprint).mkString("(", ", ", ")")} => ${to.prettyprint})"
  }

  override def flatten: Seq[Type] = Seq(this)
}

case class TClass(name: Name) extends Type {
  override def prettyprint: String = name.name
  override def flatten: Seq[Type] = Seq(this)
}

case class TName(name: Name) extends Type with Resolvable[TName.Target] {
  override def prettyprint: String = name.name
  override def flatten: Seq[Type] = Seq(this)
}

object TName {
  trait Target
}

case class TOption(ty: Type) extends Type {
  override def prettyprint: String = s"Option[${ty.prettyprint}]"
  override def flatten: Seq[Type] = ty.flatten
}
