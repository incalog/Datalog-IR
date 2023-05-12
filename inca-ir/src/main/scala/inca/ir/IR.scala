package inca.ir

case class Language(features: Set[IR]):
  def +(feature: IR): Language = Language(features + feature)
  def --(features: Set[IR]): Language = Language(features -- features)
  def includes(that: Language): Boolean = that.features.subsetOf(this.features)

object Language:
  val Datalog: Language = new Language(Set())
  def apply(features: IR*) = new Language(Set(features:_*))


case class Name(name: String) {
  override def toString: String = name
}

case class Module(name: Name, lang: Language, contents: Seq[ModuleEntry]) {
  override def toString: String = {
    val features = if lang.features.nonEmpty then s"(with ${lang.features.map(_.name).mkString(",")})" else ""
    val con = contents.mkString("\n")
    s"module $name $features\n$con"
  }
}
trait ModuleEntry


trait IR:
  val name: String = "Datalog"

  override def equals(obj: Any): Boolean = obj match
    case that: IR => this.name == that.name
    case _ => false

  override def hashCode(): Int = name.hashCode

  /** The IR language. Subclasses should override with `super.language + IRExtension` */
  def language: Language = Language.Datalog
  /** The target IR of this language. */
  def requires: Language = Language.Datalog

  trait Type
  case object TInt extends Type

  case class Relation(name: Name, params: Seq[Param], bodies: Seq[Body]) extends ModuleEntry:
    override def toString: String =
      s"$name${params.mkString("(", ", ", ")")} ${bodies.mkString("{\n", "\n} or {\n", "\n}")}"

  case class Param(name: Name, ty: Type):
    override def toString: String = s"$name: $ty"

  case class Body(atoms: Seq[Atom]):
    override def toString: String = s"${atoms.mkString("\t", "\n\t", "")}"

  trait Atom
  trait Term

  case class Eq(lhs: Term, rhs: Term) extends Atom:
    override def toString: String = s"$lhs == $rhs"
  case class Neq(lhs: Term, rhs: Term) extends Atom:
    override def toString: String = s"$lhs != $rhs"
  case class Call(name: Name, terms: Seq[Term]) extends Atom:
    override def toString: String = s"$name${terms.mkString("(", ", ", ")")})"

  case class Num(value: Int) extends Term:
    override def toString: String = s"$value"
  case class Var(name: Name) extends Term:
    override def toString: String = s"$name"

  case class Add(lhs: Term, rhs: Term) extends Term:
    override def toString: String = s"$lhs + $rhs"
  case class Mul(lhs: Term, rhs: Term) extends Term:
    override def toString: String = s"$lhs * $rhs"
  case class Abs(t: Term) extends Term:
    override def toString: String = s"abs($t)"
  case class Min(lhs: Term, rhs: Term) extends Term:
    override def toString: String = s"min($lhs, $rhs)"
  //case class Max(lhs: Term, rhs: Term) extends Term
  //case class Sub(lhs: Term, rhs: Term) extends Term
  //case class Div(lhs: Term, rhs: Term) extends Term

trait DisjunctionIR extends IR:
  override val name: String = "Disjunction"
  override def language: Language = super.language + new DisjunctionIR {}
  override def requires: Language = Language()

  case class Disjunction(as1: Seq[Atom], as2: Seq[Atom]) extends Atom:
    override def toString: String = s"${as1.mkString("(", ", ", ")")} v ${as2.mkString("(", ", ", ")")}"

trait BooleanIR extends IR:
  override val name: String = "Boolean"
  override def language: Language = super.language + new BooleanIR {}
  override def requires: Language = Language()

  case object TBoolean extends Type
  case class BoolAtom(t: Term) extends Atom:
    override def toString: String = s"$t"
  case class BoolAnd(t1: Term, t2: Term) extends Term:
    override def toString: String = s"$t1 && $t2"
  case class BoolOr(t1: Term, t2: Term) extends Term:
    override def toString: String = s"$t1 || $t2"
  case class BoolNot(t: Term) extends Term:
    override def toString: String = s"!$t"
  case object BoolTrue extends Term:
    override def toString: String = "true"
  case object BoolFalse extends Term:
    override def toString: String = "false"

trait TupleIR extends IR:
  override val name: String = "Tuple"
  override def language: Language = super.language + new TupleIR {}
  override def requires: Language = Language()

  case class TTuple(tys: Seq[Type]) extends Type

  case class Tuple(ts: Seq[Term]) extends Term
  case class Project(t: Term, idx: Int) extends Term


trait SetIR extends IR:
  override val name: String = "Set"
  override def language: Language = super.language + new SetIR {}
  override def requires: Language = Language()

  case class TSet(ty: Type) extends Type

  case class Set(ts: Seq[Term]) extends Term
  case class SetUnion(t1: Term, t2: Term) extends Term
  case class SetIntersection(t1: Term, t2: Term) extends Term

  // Set comprehension etc. ?