package inca.ir

import inca.ir.*
import inca.ir.analysis.Analyzable
import inca.ir.printer.IRDatalogPrinter
import inca.ir.typing.{Mode, Resolvable, Typeable}
import inca.ir.util.SourceLocation

import scala.language.implicitConversions
import scala.util.NotGiven

implicit def string2name(string: String): Name = Name(string)
implicit def stringList2nameList(strings: Seq[String]): Seq[Name] = strings.map(Name.apply)
implicit def name2string(name: Name): String = name.toString
implicit def term2Arg(term: Term): Arg = term.arg
implicit def termList2ArgList(terms: Seq[Term]): Seq[Arg] = terms.map(_.arg)

// Always use the DatalogPrinter for toString!
//  To manipulate the console output change the logger in CompiledUnit by providing an implicit Printer.
private val defaultPrinter = new IRDatalogPrinter {}

case class Name(name: String) extends SourceLocation:
  override def toString: String =
    defaultPrinter.prettyPrint(this)

case class Module(name: Name, lang: Language, contents: Seq[ModuleEntry]) extends SourceLocation with Hints:
  override def toString: String = defaultPrinter.prettyPrint(this)

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

  override def toString: String = defaultPrinter.prettyPrint(this)

// Module system

trait Provide[T <: ModuleEntry] extends ModuleEntry:
  def exportRef: Ref[T]

  override val name: Name = exportRef.name

trait Require extends ModuleEntry

trait Substitution[T <: Require, S <: ModuleEntry] extends SourceLocation:
  def to: Ref[T]
  def from: Ref[S]

  override def toString: String = defaultPrinter.prettyPrint(this)

case class Import(module: Ref[Module], as: Name, subst: Seq[Substitution[?, ?]]) extends ModuleEntry:
  override val name: Name = Name(s"Import ${module.name} as $as")

  def withName(name: String): ModuleEntry = this.copy(as = Name(name))

object Import:
  def apply(module: Name, as: Name) = new Import(RefByName(module), as, Seq())

  def apply(module: Name, as: Name, entries: Seq[Substitution[?, ?]]) = new Import(RefByName(module), as, entries)


// relation specific module system

case class RequireRelation(name: Name, params: Seq[Param]) extends RelationBase, Require:
  def withName(name: String): ModuleEntry = this.copy(name = Name(name))

case class RequireExtensionalRelation(name: Name, params: Seq[Param]) extends ExtensionalRelationBase, Require:
  def withName(name: String): ModuleEntry = this.copy(name = Name(name))

case class ProvideRelation(exportRef: Ref[RelationBase], params: Seq[Param]) extends RelationBase, Provide[RelationBase]:
  def withName(name: String): ModuleEntry =
    val newRef = RefByName[RelationBase](name)
    newRef.target = exportRef.target
    this.copy(exportRef = newRef)

object ProvideRelation:
  def apply(exportName: Name, params: Seq[Param]): ProvideRelation =
    new ProvideRelation(RefByName(exportName), params)

case class ProvideExtensionalRelation(exportRef: Ref[ExtensionalRelationBase], params: Seq[Param]) extends ExtensionalRelationBase, Provide[ExtensionalRelationBase]:
  def withName(name: String): ModuleEntry =
    val newRef = RefByName[ExtensionalRelationBase](name)
    newRef.target = exportRef.target
    this.copy(exportRef = newRef)

object ProvideExtensionalRelation:
  def apply(exportName: Name, params: Seq[Param]): ProvideExtensionalRelation =
    new ProvideExtensionalRelation(RefByName(exportName), params)

case class RelationSubstitution(to: Ref[RequireRelation], toParams: Seq[Param], from: Ref[RelationBase], fromParams: Seq[Param]) extends Substitution[RequireRelation, RelationBase]

object RelationSubstitution:
  def apply(to: Name, toParams: Seq[Param], from: Seq[Name], fromParams: Seq[Param]): RelationSubstitution =
    if from.isEmpty then
      throw IllegalStateException("Path to a relation must not be empty")
    new RelationSubstitution(RefByName(to), fromParams, RefByQualifiedName(from), toParams)

case class ExtensionalRelationSubstitution(to: Ref[RequireExtensionalRelation], toParams: Seq[Param], from: Ref[ExtensionalRelationBase], fromParams: Seq[Param]) extends Substitution[RequireExtensionalRelation, ExtensionalRelationBase]

object ExtensionalRelationSubstitution:
  def apply(to: Name, toParams: Seq[Param], from: Seq[Name], fromParams: Seq[Param]): ExtensionalRelationSubstitution =
    if from.isEmpty then
      throw IllegalStateException("Path to a relation must not be empty")
    new ExtensionalRelationSubstitution(RefByName(to), fromParams, RefByQualifiedName(from), toParams)

// IR

trait Ref[Target] extends Resolvable[Target] with Hints with SourceLocation:
  override def toString: String = defaultPrinter.prettyPrint(this)

  def name: Name

  def unqualifiedName: Name

  def path: Seq[Name]

case class RefByName[Target](name: Name) extends Ref[Target]:
  override def unqualifiedName: Name = name

  override def path: Seq[Name] = Seq()

case class RefByQualifiedName[Target](ns: Seq[Name]) extends Ref[Target]:
  override def name: Name = Name(ns.mkString("."))

  override def unqualifiedName: Name = ns.last

  override def path: Seq[Name] = ns.dropRight(1)


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

  override def toString: String = defaultPrinter.prettyPrint(this)

case class TermArg(t: Term) extends Arg:
  def vars: Seq[Var] = t.vars

// We still need type information on wildcards for lowerings (e.g. Tuple)
case class WildcardArg() extends Arg with Typeable[TermType] with Analyzable:
  def vars: Seq[Var] = Seq()

case class TermType(ty: Type, mode: Mode):
  override def toString: String = defaultPrinter.prettyPrint(this)

case class Relation(name: Name, params: Seq[Param], bodies: Seq[Body]) extends ModuleEntry, RelationBase:
  def withName(name: String): Relation = this.copy(name = Name(name))
  def signature: Seq[Type] = params.map(_.ty)
  def isEmpty: Boolean = bodies.isEmpty || bodies.forall(_.atoms.isEmpty)
  def nonEmpty: Boolean = !isEmpty

case class ExtensionalRelation(name: Name, params: Seq[Param]) extends ModuleEntry, ExtensionalRelationBase:
  def withName(name: String): ExtensionalRelation = this.copy(name = Name(name))
  def signature: Seq[Type] = params.map(_.ty)

case class Param(name: Name, ty: Type) extends SourceLocation with Var.Target with Hints:
  override def toString: String = defaultPrinter.prettyPrint(this)

case class Body(atoms: Seq[Atom]) extends Analyzable, Hints:
  override def toString: String = defaultPrinter.prettyPrint(this)
  def vars: Seq[Var] = atoms.flatMap(_.vars)

case class Var(ref: Ref[Var.Target]) extends Term with Var.Target:
  def name: Name = ref.name
  override def vars: Seq[Var] = Seq(this)

object Var:
  def apply(name: Name): Var = new Var(RefByName(name))

  trait Target extends SourceLocation

case class Cast(t: Term, ty: Type) extends Term:
  override def vars: Seq[Var] = t.vars

trait RelationBase extends ModuleEntry

case class Call(ref: Ref[? <: RelationBase], args: Seq[Arg], neg: Boolean) extends Atom:
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
  override def vars: Seq[Var] = args.flatMap(_.vars)

object ExtensionalCall:
  def apply(name: Name, args: Seq[Arg], neg: Boolean = false): ExtensionalCall =
    ExtensionalCall(RefByName(name), args, neg)

case class Eq(lhs: Term, rhs: Term, neg: Boolean = false) extends Atom:
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
