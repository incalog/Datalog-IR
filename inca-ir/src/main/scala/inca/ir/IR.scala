package inca.ir

case class Language(features: Set[IR]):
  def +(feature: IR): Language = Language(features + feature)
  def includes(that: Language): Boolean = that.features.subsetOf(this.features)
object Language:
  val Datalog: Language = new Language(Set())
  def apply(features: IR*) = new Language(Set(features:_*))


case class Name(name: String) {
  override def toString: String = name
}

case class Module(name: Name, lang: Language, contents: List[ModuleEntry])
trait ModuleEntry

trait IR:
  val name: String = "Datalog"

  override def equals(obj: Any): Boolean = obj match
    case that: IR => this.name == that.name
    case _ => false
  override def hashCode(): Int = name.hashCode

  /** The IR language. Subclasses should override with `super.language + new IRExtension {}`` */
  def language: Language = Language.Datalog
  /** The target IR of this language. */
  def requires: Language = Language.Datalog

  trait Type
  case object TInt extends Type

  case class Relation(name: Name, params: Seq[Param], bodies: Seq[Body]) extends ModuleEntry
  case class Param(name: Name, ty: Type)
  case class Body(atoms: List[Atom])
  trait Atom
  trait Term




trait DisjunctionIR extends IR:
  override val name: String = "Disjunction"
  override def language: Language = super.language + new DisjunctionIR {}
  override def requires: Language = Language()

  case class Disjunction(as1: List[Atom], as2: List[Atom]) extends Atom


trait BooleanIR extends IR:
  override val name: String = "Booleans"
  override def language: Language = super.language + new BooleanIR {}
  override def requires: Language = Language()

  case object TBoolean extends Type

  case class BoolAtom(t: Term) extends Atom
  case class BoolAnd(t1: Term, t2: Term) extends Term
  case class BoolOr(t1: Term, t2: Term) extends Term
  case class BoolNot(t: Term) extends Term
