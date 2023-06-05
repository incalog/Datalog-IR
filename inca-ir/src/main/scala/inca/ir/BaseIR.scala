package inca.ir

import inca.ir.*
import inca.ir.extensions.*

case class Name(name: String):
  override def toString: String = name
  def byAppending(suffix: String): Name = Name(this.name + suffix)

case class Module(name: Name, lang: Language, contents: Seq[ModuleEntry]):
  override def toString: String = {
    val features = if lang.features.nonEmpty then s"(with ${lang.features.map(_.name).mkString(",")})" else ""
    val con = contents.mkString("\n")
    s"module $name $features\n$con"
  }

trait ModuleEntry



trait Atom
trait Term
trait Type

case class Relation(name: Name, params: Seq[Param], bodies: Seq[Body]) extends ModuleEntry:
  override def toString: String =
    s"$name${params.mkString("(", ", ", ")")} ${bodies.mkString("{\n", "\n} or {\n", "\n}")}"

case class Param(name: Name, ty: Type):
  override def toString: String = s"$name: $ty"

case class Body(atoms: Seq[Atom]):
  override def toString: String = s"${atoms.mkString("\t", "\n\t", "")}"

case class Var(name: Name) extends Term:
  override def toString: String = s"$name"

case class Call(name: Name, args: Seq[Term]) extends Atom:
  override def toString: String = s"$name${args.mkString("(", ", ", ")")}"

case class ExtensionalCall(name: Name, args: Seq[Term], neg: Boolean = false) extends Atom:
  override def toString: String =
    if (neg)
      s"ext !$name${args.mkString("(", ", ", ")")}"
    else
      s"ext $name${args.mkString("(", ", ", ")")}"

case class Eq(lhs: Term, rhs: Term) extends Atom:
  override def toString: String = s"$lhs == $rhs"

case class Neq(lhs: Term, rhs: Term) extends Atom:
  override def toString: String = s"$lhs != $rhs"

case object TAny extends Type

//case object TInt extends Type

trait BaseIR:
  val name: String = "Datalog"

  override def equals(obj: Any): Boolean = obj match
    case that: BaseIR => this.name == that.name
    case _ => false

  override def hashCode(): Int = name.hashCode

  /** The IR language. Subclasses should override with `super.language + IRExtension` */
  def language: Language = Language.Datalog
  /** The target IR of this language. */
  def requires: Language = Language.Datalog

