package inca.frontend.functional.syntax

import inca.ir.Name
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
object TFun:
  def apply(from: Type, to: Type): TFun = from match
    case TTuple(ts) => TFun(ts, to)
    case _ => TFun(Seq(from), to)

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
  def isBuiltIn: Boolean = builtInTypes.contains(name.name)
}
val builtInTypes = Set("Int", "Boolean", "String", "Double")
def TInt: Type = TName(Name("Int"))
def TDouble: Type = TName(Name("Double"))
def TBoolean: Type = TName(Name("Boolean"))
def TString: Type = TName(Name("String"))

object TName {
  trait Target
}

case class TApply(op: Type, tys: Seq[Type]) extends Type with Resolvable[TName.Target] {
  override def prettyprint: String =
    s"${op.prettyprint}" + (if(tys.isEmpty) "" else s"[${tys.map(_.prettyprint).mkString(", ")}]")
  override def flatten: Seq[Type] = Seq(this)
  // TODO fix?
  override def freeTvars: Seq[TName] = op.freeTvars ++ tys.flatMap(_.freeTvars)
}

case class TSet(ty: Type) extends Type {
  override def prettyprint: String = s"Set[${ty.prettyprint}]"
  override def flatten: Seq[Type] = ty.flatten
  override def freeTvars: Seq[TName] = ty.freeTvars
}
