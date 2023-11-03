package inca.frontend.oodl.syntax

import inca.ir.Name
import inca.ir.typing.Resolvable
import inca.ir.util.SourceLocation

sealed trait Type extends SourceLocation {
  def prettyprint: String
  def flatten: Seq[Type]
  override def toString: String = prettyprint
}
case object TAny extends Type {
  override def prettyprint: String = "Any"
  override def flatten: Seq[Type] = Seq(this)
}

case object TNull extends Type {
  override def prettyprint: String = "Null"
  override def flatten: Seq[Type] = Seq(this)
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
}
object TTuple {
  def from(ts: Seq[Type]): Type = ts match {
    case Nil => TUnit
    case t :: Nil => t
    case ts => TTuple(ts)
  }
}

case class TName(name: Name, tyArgs: Seq[Type]) extends Type with Resolvable[TName.Target]:
  override def prettyprint: String = name.name
  override def flatten: Seq[Type] = Seq(this)
  def isBuiltIn: Boolean = builtInTypes.contains(name.name)

val builtInTypes = Set("Int", "Boolean", "String", "Double")
def TInt: Type = TName(Name("Int"), Seq())
def TDouble: Type = TName(Name("Double"), Seq())
def TBoolean: Type = TName(Name("Boolean"), Seq())
def TString: Type = TName(Name("String"), Seq())

object TName {
  trait Target
}

case class TSet(ty: Type) extends Type {
  override def prettyprint: String = s"Set[${ty.prettyprint}]"
  override def flatten: Seq[Type] = ty.flatten
}