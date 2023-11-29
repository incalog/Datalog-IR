package inca.ir

import inca.ir.*
import inca.ir.analysis.Analyzable
import inca.ir.typing.{Mode, Typeable}
import inca.ir.util.SourceLocation

import scala.language.implicitConversions

implicit def string2name(string: String): Name = Name(string)
implicit def name2string(name: Name): String = name.toString

implicit def term2Arg(term: Term): Arg = term.arg

case class Name(name: String) extends SourceLocation:
  override def toString: String = name

case class Module(name: Name, lang: Language, contents: Seq[ModuleEntry]) extends SourceLocation:
  override def toString: String = {
    val features = if lang.features.nonEmpty then s"(with ${lang.features.map(_.name).mkString(",")})" else ""
    val con = contents.mkString("\n")
    s"module $name $features\n$con"
  }

  lazy val relations: Map[String,Relation] = contents.collect { case r: Relation => (r.name.name,r) }.toMap

trait ModuleEntry extends SourceLocation with Hints:
  val name: Name


trait Atom extends Analyzable with SourceLocation with Hints:
  def vars: Seq[Var]

trait Term extends Typeable[TermType] with Analyzable with SourceLocation with Hints:
  def vars: Seq[Var]
  def mode: Mode = this.typ.getOrElse(throw new IllegalStateException(s"untyped $this")).mode
  def arg: Arg = TermArg(this)

trait Type extends SourceLocation with Hints:
  def size: Int = 1
  def flatten: Seq[Type] = Seq(this)

  def bound: TermType = TermType(this, Mode.Bound)
  def binding: TermType = TermType(this, Mode.Binding)
  def collapsed: TermType = TermType(this, Mode.Collapse)

trait Arg extends SourceLocation:
  def vars: Seq[Var]

case class TermArg(t: Term) extends Arg:
  def vars: Seq[Var] = t.vars
  override def toString: String = t.toString

// We still need type information on wildcards for lowerings (e.g. Tuple)
case class WildcardArg() extends Arg with Typeable[TermType]:
  def vars: Seq[Var] = Seq()
  override def toString: String = "_"

case class TermType(ty: Type, mode: Mode):
  override def toString: String =
    if (mode.isBound)
      s"<$ty>"
    else if (mode.isBinding)
      s">$ty<"
    else if (mode.isCollapse)
      s"<_>"
    else
      throw IllegalStateException(s"Unknown mode $mode")

case class Relation(name: Name, params: Seq[Param], bodies: Seq[Body]) extends ModuleEntry:
  override def toString: String = {
    val prefix = s"$name${params.mkString("(", ", ", ")")}"
    if (bodies.isEmpty)
      s"$prefix = nil"
    else
      s"$prefix ${bodies.mkString("{\n", "\n} or {\n", "\n}")}"
  }
  def signature: Seq[Type] = params.map(_.ty)
  def isEmpty: Boolean = bodies.isEmpty || bodies.forall(_.atoms.isEmpty)
  def nonEmpty: Boolean = !isEmpty

case class ExtensionalRelation(name: Name, params: Seq[Param]) extends ModuleEntry:
  override def toString: String = s"ext $name${params.mkString("(", ", ", ")")} = nil"
  def signature: Seq[Type] = params.map(_.ty)

case class Param(name: Name, ty: Type) extends SourceLocation with Var.Target with Hints:
  override def toString: String = s"$name: $ty"

case class Body(atoms: Seq[Atom]) extends Hints:
  override def toString: String = s"${atoms.mkString("\t", "\n\t", "")}"
  def vars: Seq[Var] = atoms.flatMap(_.vars)

case class Var(name: Name) extends Term with Var.Target:
  override def toString: String =
    if (typ.isEmpty)
      s"$name" + analysisString
    else
      s"$name: ${typ.get}" + analysisString
  override def vars: Seq[Var] = Seq(this)

object Var:
  trait Target extends SourceLocation

case class Cast(t: Term, ty: Type) extends Term:
  override def toString: String =
    if (t.typ.exists(_.ty == ty))
      t.toString
    else
      s"$t: $ty"
  override def vars: Seq[Var] = t.vars

case class Call(name: Name, args: Seq[Arg], neg: Boolean = false) extends Atom:
  override def toString: String =
    val negPrefix = if (neg) "~" else ""
    s"$negPrefix$name${args.mkString("(", ", ", ")")}" + analysisString
  override def vars: Seq[Var] = args.flatMap(_.vars)

case class ExtensionalCall(name: Name, args: Seq[Arg], neg: Boolean = false) extends Atom:
  override def toString: String =
    val negPrefix = if (neg) "~" else ""
    s"ext $negPrefix$name${args.mkString("(", ", ", ")")}" + analysisString
  override def vars: Seq[Var] = args.flatMap(_.vars)

case class Eq(lhs: Term, rhs: Term, neg: Boolean = false) extends Atom:
  override def toString: String =
    val op = if (neg) "!=" else "=="
    s"$lhs $op $rhs" + analysisString
  override def vars: Seq[Var] = lhs.vars ++ rhs.vars

case object TAny extends Type
case object TNothing extends Type


trait BaseIR:
  val name: String = "Datalog"

  override def equals(obj: Any): Boolean = obj match
    case that: BaseIR => this.name == that.name
    case _ => false

  override def hashCode(): Int = name.hashCode
//  override def toString: String = language.toString

  /** The IR language. Subclasses should override with `super.language + IRExtension` */
  def language: Language = Language.Datalog
  /** The target IR of this language. */
  def requires: Language = Language.Datalog
object BaseIR extends BaseIR {}
