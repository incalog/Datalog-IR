package inca.ir

import inca.ir.*
import inca.ir.typing.{Typeable, Mode}
import inca.ir.util.SourceLocation

import scala.language.implicitConversions

implicit def string2name(string: String): Name = Name(string)
implicit def name2string(name: Name): String = name.toString

case class Name(name: String) extends SourceLocation:
  override def toString: String = name

case class Module(name: Name, lang: Language, contents: Seq[ModuleEntry]) extends SourceLocation:
  override def toString: String = {
    val features = if lang.features.nonEmpty then s"(with ${lang.features.map(_.name).mkString(",")})" else ""
    val con = contents.mkString("\n")
    s"module $name $features\n$con"
  }

  lazy val relations: Map[String,Relation] = contents.collect { case r: Relation => (r.name.name,r) }.toMap

trait ModuleEntry(val name: Name) extends SourceLocation with Hints



trait Atom extends SourceLocation with Hints
trait Term extends Typeable[TermType] with SourceLocation with Hints:
  def vars: Seq[Var] = Seq()

trait Type extends SourceLocation with Hints:
  def size: Int = 1
  def flatten: Seq[Type] = Seq(this)

  def closed: TermType = TermType(this, Mode.Bound)
  def closing: TermType = TermType(this, Mode.Binding)

case class TermType(ty: Type, mode: Mode):
  override def toString: String = mode match
    case Mode.Binding => s">$ty<"
    case Mode.Bound => s"<$ty>"

case class Relation(override val name: Name, params: Seq[Param], bodies: Seq[Body]) extends ModuleEntry(name):
  override def toString: String = {
    val prefix = s"$name${params.mkString("(", ", ", ")")}"
    if (bodies.isEmpty)
      s"$prefix = nil"
    else
      s"$prefix ${bodies.mkString("{\n", "\n} or {\n", "\n}")}"
  }
  def signature: Seq[Type] = params.map(_.ty)

case class Param(name: Name, ty: Type) extends SourceLocation with Var.Target with Hints:
  override def toString: String = s"$name: $ty"

case class Body(atoms: Seq[Atom]) extends Hints:
  override def toString: String = s"${atoms.mkString("\t", "\n\t", "")}"

case class Var(name: Name) extends Term with Var.Target:
  override def toString: String =
    if (typ.isEmpty)
      s"$name"
    else
      s"$name: ${typ.get}"
  override def vars: Seq[Var] = Seq(this)

object Var {
  //def apply(name: String): Var = new Var(Name(name))
  trait Target extends SourceLocation
}

case class Cast(t: Term, ty: Type) extends Term:
  override def toString: String = s"$t:$ty"
  override def vars: Seq[Var] = t.vars

case class Call(name: Name, args: Seq[Term]) extends Atom:
  override def toString: String = s"$name${args.mkString("(", ", ", ")")}"

case class NegCall(name: Name, args: Seq[Term]) extends Atom:
  override def toString: String = s"!$name${args.mkString("(", ", ", ")")}"

case class ExtensionalCall(name: Name, args: Seq[Term]) extends Atom:
  override def toString: String = s"ext $name${args.mkString("(", ", ", ")")}"

case class NegExtensionalCall(name: Name, args: Seq[Term]) extends Atom:
  override def toString: String = s"ext !$name${args.mkString("(", ", ", ")")}"

case class Eq(lhs: Term, rhs: Term) extends Atom:
  override def toString: String = s"$lhs == $rhs"

case class Neq(lhs: Term, rhs: Term) extends Atom:
  override def toString: String = s"$lhs != $rhs"

case object TAny extends Type
case object TNothing extends Type

//case object TInt extends Type

trait BaseIR:
  val name: String = "Datalog"

  override def equals(obj: Any): Boolean = obj match
    case that: BaseIR => this.name == that.name
    case _ => false

  override def hashCode(): Int = name.hashCode
  override def toString: String = language.toString

  /** The IR language. Subclasses should override with `super.language + IRExtension` */
  def language: Language = Language.Datalog
  /** The target IR of this language. */
  def requires: Language = Language.Datalog
object BaseIR extends BaseIR {}
