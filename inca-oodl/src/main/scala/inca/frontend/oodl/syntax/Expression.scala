package inca.frontend.oodl.syntax

import inca.ir.Name
import inca.ir.typing.{Resolvable, TypeCastable}
import inca.ir.util.SourceLocation

sealed trait Expression extends TypeCastable[Type] with SourceLocation {
  def vars: Map[Name, Option[Type]]

  def prettyprint(infixParens: Boolean)(implicit indent: String): String
  def prettyprint(implicit indent: String): String = prettyprint(infixParens = false)(indent)
  override def toString: String = prettyprint("")

  def infix(infixParens: Boolean)(f: => String): String =
    if (infixParens)
      s"($f)"
    else
      f
}

case class Var(name: Name) extends Expression with Resolvable[Var.Target] {
  override def vars: Map[Name, Option[Type]] = Map(name -> typ)
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = name.name
}
object Var {
  def apply(name: String): Var = new Var(Name(name))
  trait Target extends SourceLocation
}

// Use this for FieldRead / TupleRead and possible MethodCall
case class Select(recv: Expression, targetName: Name) extends Expression with Resolvable[(ClassDef, FieldDef)]:
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = s"$recv.$targetName"
  override def vars: Map[Name, Option[Type]] = recv.vars

case class ConstructorCall(name: Name, tyArgs: Seq[Type], args: Seq[Expression]) extends Expression with Resolvable[(ClassDef, ConstructorDef)]:
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    val tyS = if (tyArgs.nonEmpty) tyArgs.mkString("[", ", ", "]") else ""
    val argS = args.mkString("(", ", ", ")")
    s"new $name$tyS$argS"
  override def vars: Map[Name, Option[Type]] = args.flatMap(_.vars).toMap

case class MethodCall(recv: Expression, fun: Name, tyArgs: Seq[Type], args: Seq[Expression], isFix: Boolean = false) extends Expression with Resolvable[(ClassDef, MethodDef)]:
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    val tyS = if (tyArgs.nonEmpty) tyArgs.mkString("[", ", ", "]") else ""
    val argS = args.mkString("(", ", ", ")")
    s"$recv.$fun$tyS$argS"
  override def vars: Map[Name, Option[Type]] = args.flatMap(_.vars).toMap

case class TypeCast(recv: Expression, toTyp: Type) extends Expression:
  override def vars: Map[Name, Option[Type]] = recv.vars
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = s"$recv.asInstanceOf[$toTyp]"

case class InstanceOf(recv: Expression, ofTyp: Type) extends Expression:
  override def vars: Map[Name, Option[Type]] = recv.vars
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = s"$recv.isInstanceOf[$ofTyp]"

case class TupleExp(exps: Seq[Expression]) extends Expression:
  override def vars: Map[Name, Option[Type]] = exps.flatMap(_.vars).toMap
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    exps.map(_.prettyprint).mkString("(", ", ", ")")

case class SetExp(exps: Seq[Expression], tty: Option[Type] = None) extends Expression:
  def vars: Map[Name, Option[Type]] = exps.flatMap(_.vars).toMap
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    val tyArgs = if (tty.isDefined) s"[${tty.get}]" else ""
    exps.map(_.prettyprint).mkString(s"Set$tyArgs(", ", ", ")")

case class SetMember(name: Name, recv: Expression, predicate: Option[Expression]) extends Expression with Var.Target:
  def vars: Map[Name, Option[Type]] = recv.vars ++ (if (predicate.isDefined) predicate.get.vars else Map())
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"$name <- ${recv.prettyprint}" + (if (predicate.isDefined) s" if ${predicate.get.prettyprint}" else "")

case class SetComprehension(member: Seq[Expression], body: Expression) extends Expression:
  def vars: Map[Name, Option[Type]] = member.flatMap(_.vars).toMap ++ body.vars
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    // TODO: Fix indent
    val predS = member.map(_.prettyprint).mkString("; ")
    s"for ($predS) yield ${body.prettyprint}"
  }

// This would make problems if we try to assign unit to a variable.
// How should that look like in Datalog, since an empty Tuple is nothing.
// We could prevent this by forcing Block to always have a result expression, but this is not what we want for an if
//case class Block(stats: Seq[Stat], result: Expression) extends Expression

case class NullLit() extends Expression:
  override def vars: Map[Name, Option[Type]] = Map()
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = "Null"

case class BoolLit(b: Boolean) extends Expression:
  override def vars: Map[Name, Option[Type]] = Map()
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = s"$b"

case class IntLit(i: Int) extends Expression:
  override def vars: Map[Name, Option[Type]] = Map()
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = s"$i"

case class DoubleLit(d: Double) extends Expression:
  override def vars: Map[Name, Option[Type]] = Map()
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = s"$d"

case class StringLit(s: String) extends Expression:
  override def vars: Map[Name, Option[Type]] = Map()
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = s""""$s""""

case class BinOp(e1: Expression, op: String, e2: Expression) extends Expression:
  override def vars: Map[Name, Option[Type]] = Map()
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = s"$e1 $op $e2"

case class UnOp(op: String, e: Expression) extends Expression:
  override def vars: Map[Name, Option[Type]] = Map()
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = s"$op $e"


// TODO: Pattern matching