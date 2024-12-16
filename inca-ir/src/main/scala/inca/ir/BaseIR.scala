package inca.ir

import inca.ir.*
import inca.ir.analysis.Analyzable
import inca.ir.typing.{Mode, Resolvable, Typeable}
import inca.ir.util.SourceLocation
import inca.util.datastructures.Graph

import java.lang.annotation.Target
import scala.language.implicitConversions

implicit def string2name(string: String): Name = Name(string)
implicit def stringList2nameList(strings: Seq[String]): Seq[Name] = strings.map(Name.apply)
implicit def name2string(name: Name): String = name.toString
implicit def term2Arg(term: Term): Arg = term.arg
implicit def termList2ArgList(terms: Seq[Term]): Seq[Arg] = terms.map(_.arg)

case class Name(name: String) extends SourceLocation:
  override def toString: String = name


case class Module(name: Name, lang: Language, contents: Seq[ModuleEntry]) extends SourceLocation with Hints:
  override def toString: String = {
    val features = if lang.features.nonEmpty then s"(with ${lang.features.map(_.name).mkString(",")})" else ""
    val con = contents.mkString("\n")
    s"module $name $features\n$con"
  }

  lazy val entries: Map[Name, ModuleEntry] = contents.map(e => e.name -> e).toMap
  lazy val imports: Seq[Import] = contents.collect { case i: Import => i }
  lazy val relations: Map[String, Relation] = contents.collect { case r: Relation => (r.name.name, r) }.toMap
  lazy val header: Module = Module(name, lang, contents.filter {
    case _: Import | _: Require | _: Provide[_] => true
    case _ => false
  })

trait ModuleEntry extends SourceLocation with Hints with Analyzable:
  val name: Name

  def withName(name: String): ModuleEntry

  def withExtendedName(suffix: String): ModuleEntry = withName(Name(name.name + suffix))


// Module system

trait Provide[T <: ModuleEntry] extends ModuleEntry:
  def exportRef: Ref[T]

  override val name: Name = exportRef.name

trait Require extends ModuleEntry

trait Substitution[T <: Require, S <: ModuleEntry] extends SourceLocation:
  def to: Ref[T]

  def from: Ref[S]

case class Import(module: Ref[Module], as: Name, subst: Seq[Substitution[?, ?]]) extends ModuleEntry:
  override val name: Name = Name(s"Import ${module.name} as $as")

  override def toString: String =
    if subst.nonEmpty then
      s"import ${module.name} as $as with { ${subst.mkString(", ")} }"
    else
      s"import ${module.name} as $as"

  def withName(name: String): ModuleEntry = this.copy(as = Name(name))

object Import:
  def apply(module: Name, as: Name) = new Import(RefByName(module), as, Seq())

  def apply(module: Name, as: Name, entries: Seq[Substitution[?, ?]]) = new Import(RefByName(module), as, entries)


// relation specific module system

case class RequireRelation(name: Name, params: Seq[Param]) extends RelationBase, Require:
  override def toString: String = s"require $name(${params.mkString(", ")})"

  def withName(name: String): ModuleEntry = this.copy(name = Name(name))

case class RequireExtensionalRelation(name: Name, params: Seq[Param]) extends ExtensionalRelationBase, Require:
  override def toString: String = s"require ext $name(${params.mkString(", ")})"

  def withName(name: String): ModuleEntry = this.copy(name = Name(name))

case class ProvideRelation(exportRef: Ref[RelationBase], params: Seq[Param]) extends RelationBase, Provide[RelationBase]:
  override def toString: String = s"provide $exportRef(${params.mkString(", ")})"

  def withName(name: String): ModuleEntry =
    val newRef = RefByName[RelationBase](name)
    newRef.target = exportRef.target
    this.copy(exportRef = newRef)

object ProvideRelation:
  def apply(exportName: Name, params: Seq[Param]): ProvideRelation =
    new ProvideRelation(RefByName(exportName), params)

case class ProvideExtensionalRelation(exportRef: Ref[ExtensionalRelationBase], params: Seq[Param]) extends ExtensionalRelationBase, Provide[ExtensionalRelationBase]:
  override def toString: String = s"provide ext $exportRef(${params.mkString(", ")})"

  def withName(name: String): ModuleEntry =
    val newRef = RefByName[ExtensionalRelationBase](name)
    newRef.target = exportRef.target
    this.copy(exportRef = newRef)

object ProvideExtensionalRelation:
  def apply(exportName: Name, params: Seq[Param]): ProvideExtensionalRelation =
    new ProvideExtensionalRelation(RefByName(exportName), params)

case class RelationSubstitution(to: Ref[RequireRelation], toParams: Seq[Param], from: Ref[RelationBase], fromParams: Seq[Param]) extends Substitution[RequireRelation, RelationBase]:
  override def toString: String = s"$to(${toParams.mkString(", ")}) = ${from.name}(${fromParams.mkString(", ")})"

object RelationSubstitution:
  def apply(to: Name, toParams: Seq[Param], from: Seq[Name], fromParams: Seq[Param]): RelationSubstitution =
    if from.isEmpty then
      throw IllegalStateException("Path to a relation must not be empty")
    new RelationSubstitution(RefByName(to), fromParams, RefByQualifiedName(from), toParams)

case class ExtensionalRelationSubstitution(to: Ref[RequireExtensionalRelation], toParams: Seq[Param], from: Ref[ExtensionalRelationBase], fromParams: Seq[Param]) extends Substitution[RequireExtensionalRelation, ExtensionalRelationBase]:
  override def toString: String = s"$to(${toParams.mkString(", ")}) = ext ${from.name}(${fromParams.mkString(", ")})"

object ExtensionalRelationSubstitution:
  def apply(to: Name, toParams: Seq[Param], from: Seq[Name], fromParams: Seq[Param]): ExtensionalRelationSubstitution =
    if from.isEmpty then
      throw IllegalStateException("Path to a relation must not be empty")
    new ExtensionalRelationSubstitution(RefByName(to), fromParams, RefByQualifiedName(from), toParams)

// IR

trait Ref[Target] extends Resolvable[Target] with Hints with SourceLocation:
  def name: Name

  def unqualifiedName: Name

  def path: Seq[Name]

case class RefByName[Target](name: Name) extends Ref[Target]:
  override def toString: String = name.name //+ ":: " +  target

  override def unqualifiedName: Name = name

  override def path: Seq[Name] = Seq()

case class RefByQualifiedName[Target](ns: Seq[Name]) extends Ref[Target]:
  override def name: Name = Name(ns.mkString("."))

  override def unqualifiedName: Name = ns.last

  override def path: Seq[Name] = ns.dropRight(1)

  override def toString: String = name.name //+ ":: " +  target

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
case class WildcardArg() extends Arg with Typeable[TermType] with Analyzable:
  def vars: Seq[Var] = Seq()

  override def toString: String =
    if (typ.isEmpty)
      s"_" //+ analysisString
    else
      s"_: ${typ.get}" //+ analysisString

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

case class Relation(name: Name, params: Seq[Param], bodies: Seq[Body]) extends ModuleEntry, RelationBase:
  def withName(name: String): Relation = this.copy(name = Name(name))

  override def toString: String = {
    val prefix = s"$name${params.mkString("(", ", ", ")")}"
    if (bodies.isEmpty)
      s"$prefix = nil" //+ analysisString
    else
      s"$prefix ${bodies.mkString("{\n", "\n} or {\n", "\n}")}" //+ analysisString
  }

  def signature: Seq[Type] = params.map(_.ty)

  def isEmpty: Boolean = bodies.isEmpty || bodies.forall(_.atoms.isEmpty)

  def nonEmpty: Boolean = !isEmpty

case class ExtensionalRelation(name: Name, params: Seq[Param]) extends ModuleEntry, ExtensionalRelationBase:
  def withName(name: String): ExtensionalRelation = this.copy(name = Name(name))

  override def toString: String = s"ext $name${params.mkString("(", ", ", ")")}" //+ analysisString

  def signature: Seq[Type] = params.map(_.ty)

case class Param(name: Name, ty: Type) extends SourceLocation with Var.Target with Hints:
  override def toString: String = s"$name: $ty"

case class Body(atoms: Seq[Atom]) extends Analyzable, Hints:
  override def toString: String = s"${atoms.mkString("\t", "\n\t", "")}" //+ analysisString

  def vars: Seq[Var] = atoms.flatMap(_.vars)

case class Var(ref: Ref[Var.Target]) extends Term with Var.Target:
  def name: Name = ref.name

  override def toString: String =
    if (typ.isEmpty)
      s"$ref" //+ analysisString
    else
      s"$ref: ${typ.get}" //+ analysisString

  override def vars: Seq[Var] = Seq(this)

object Var:
  def apply(name: Name): Var = new Var(RefByName(name))

  trait Target extends SourceLocation

case class Cast(t: Term, ty: Type) extends Term:
  override def toString: String =
    if (t.typ.exists(_.ty == ty))
      t.toString
    else
      s"$t: $ty"

  override def vars: Seq[Var] = t.vars

trait RelationBase extends ModuleEntry

case class Call(ref: Ref[? <: RelationBase], args: Seq[Arg], neg: Boolean) extends Atom:
  override def toString: String =
    val negPrefix = if (neg) "~" else ""
    s"$negPrefix$ref${args.mkString("(", ", ", ")")}" //+ analysisString

  override def vars: Seq[Var] = args.flatMap(_.vars)

object Call:
  def apply(name: Name, args: Seq[Arg], neg: Boolean = false): Call = Call(RefByName(name), args, neg)

  def apply(qname: Seq[Name], args: Seq[Arg]): Call =
    if qname.size == 1 then
      Call(RefByName(qname.last), args, false)
    else
      Call(RefByQualifiedName(qname), args, false)

object NegCall:
  def apply(name: Name, args: Seq[Arg]): Call = Call(RefByName(name), args, true)

  def apply(qname: Seq[Name], args: Seq[Arg]): Call =
    if qname.size == 1 then
      Call(RefByName(qname.last), args, true)
    else
      Call(RefByQualifiedName(qname), args, true)


trait ExtensionalRelationBase extends ModuleEntry

case class ExtensionalCall(ref: Ref[? <: ExtensionalRelationBase], args: Seq[Arg], neg: Boolean) extends Atom:
  override def toString: String =
    val negPrefix = if (neg) "~" else ""
    s"ext $negPrefix$ref${args.mkString("(", ", ", ")")}" //+ analysisString

  override def vars: Seq[Var] = args.flatMap(_.vars)

object ExtensionalCall:
  def apply(name: Name, args: Seq[Arg], neg: Boolean = false): ExtensionalCall =
    ExtensionalCall(RefByName(name), args, neg)

case class Eq(lhs: Term, rhs: Term, neg: Boolean = false) extends Atom:
  override def toString: String =
    val op = if (neg) "!=" else "=="
    s"$lhs $op $rhs" //+ analysisString

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
