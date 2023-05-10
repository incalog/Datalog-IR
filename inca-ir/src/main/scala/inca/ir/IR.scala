package inca.ir

case class Language(features: Set[IR]):
  def +(feature: IR): Language = Language(features + feature)
  def -(feature: IR): Language = Language(features - feature)
  def includes(that: Language): Boolean = that.features.subsetOf(this.features)

object Language:
  val Datalog: Language = new Language(Set())
  def apply(features: IR*) = new Language(Set(features:_*))


case class Name(name: String) {
  override def toString: String = name
}

case class Module(name: Name, lang: Language, contents: Seq[ModuleEntry])
trait ModuleEntry


trait IR:
  val name: String
  /** The IR language. Subclasses should override with `super.language + IRExtension`` */
  def language: Language = Language.Datalog
  /** The target IR of this language. */
  def requires: Language = Language.Datalog


object IR extends IR:
  override val name: String = "Datalog"

  trait Type
  case object TInt extends Type

  case class Relation(name: Name, params: Seq[Param], bodies: Seq[Body]) extends ModuleEntry
  case class Param(name: Name, ty: Type)
  case class Body(atoms: List[Atom])
  trait Atom
  trait Term

trait DisjunctionIR extends IR:
  override val name: String = "Disjunction"
  override def language: Language = super.language + DisjunctionIR
  override def requires: Language = Language()

object DisjunctionIR extends DisjunctionIR:
  case class Disjunction(as1: List[IR.Atom], as2: List[IR.Atom]) extends IR.Atom


trait BooleanIR extends IR:
  override val name: String = "Booleans"
  override def language: Language = super.language + BooleanIR
  override def requires: Language = Language()

object BooleanIR extends BooleanIR:
  case object TBoolean extends IR.Type
  // I'm confused about this... What should this atom represent ?
  case class BoolAtom(t: IR.Term) extends IR.Atom
  case class BoolAnd(t1: IR.Term, t2: IR.Term) extends IR.Term
  case class BoolOr(t1: IR.Term, t2: IR.Term) extends IR.Term
  case class BoolNot(t: IR.Term) extends IR.Term


trait TupleIR extends IR:
  override val name: String = "Tuples"
  override def language: Language = super.language + TupleIR
  override def requires: Language = Language()

object TupleIR extends TupleIR:
  case class TTuple(tys: Seq[IR.Type]) extends IR.Type
  // TODO: This should be a term, since we can view a tuple as a constant
  case class TupleAtom(ts: Seq[IR.Term]) extends IR.Atom
  case class TupleRead(t: IR.Term, index: Int) extends IR.Term


trait SetIR extends IR:
  override val name: String = "Sets"
  override def language: Language = super.language + SetIR
  override def requires: Language = Language()

object SetIR extends SetIR:
  case class TSet(ty: IR.Type) extends IR.Type

  case class SetAtom(ts: Seq[IR.Term]) extends IR.Atom

  // Set comprehension etc. ?