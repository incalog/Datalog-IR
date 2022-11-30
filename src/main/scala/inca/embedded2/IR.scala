package inca.embedded2

trait DB {
  def relation(name: String): Relation
}

trait Relation

trait IR[FL <: Language[_]] extends Language[FL] {
  val fl: FL
  type Value = DB

  type Program = Module

  case class Module(
      name: String,
      imports: Seq[String],
      patterns: Seq[Pattern],
      flTopLevel: Seq[fl.TopLevelDefinition])
  case class Parameter(name: String, ty: Type)
  case class Pattern(name: String, params: Seq[Parameter], bodies: Seq[Body])

  trait Type
  case class FLType(ty: fl.Type) extends Type

  case class Body(atoms: Seq[Atom])
  trait Atom
  case class Call(name: String, args: Seq[Term], neg: Boolean = false)
  case class ExtensionalCall(name: String, args: Seq[Term], neg: Boolean = false)
  trait Comparator
  case object EqComparator extends Comparator
  case object NeqComparator extends Comparator
  case class Compare(comp: Comparator, lhs: Term, rhs: Term) extends Atom
  def Eq(lhs: Term, rhs: Term): Compare = Compare(EqComparator, lhs, rhs)
  def Neq(lhs: Term, rhs: Term): Compare = Compare(NeqComparator, lhs, rhs)

  case class Compute(lhs: Term, computation: Computation) extends Atom
  trait Computation
  case class FLApp(name: fl.Function, args: Seq[Term]) extends Computation
  case class FLInfixApp(lhs: Term, op: fl.InfixOperator, rhs: Term) extends Computation
  case class UnaryApp(op: fl.UnaryOperator, t: Term) extends Computation
  case class Count(pattern: String, args: Seq[Term]) extends Computation
  case class Aggregation(
      ty: Type,
      f: fl.Function,
      pattern: String,
      args: Seq[Term],
      aggregationColumn: Int)
      extends Computation

  trait Term
  case class FLConst(c: fl.Constant) extends Term
  case class Var(name: String) extends Term
}

trait ASTIR[FL <: Language[_]] extends IR[FL] {

  case class NodeType(name: String) extends Type
  case class ListType(inner: Type) extends Type
  case class OptionType(inner: Type) extends Type

  trait LiteralType extends Type
  case object IntegerLiteral extends LiteralType
  case object LongLiteral extends LiteralType
  case object FloatLiteral extends LiteralType
  case object DoubleLiteral extends LiteralType
  case object StringLiteral extends LiteralType
  case object BooleanLiteral extends LiteralType

  sealed trait Link
  case object ParentLink extends Link
  case object NextLink extends Link
  case object SizeLink extends Link
  case class NamedLink(node: NodeType, field: String) extends Link

  trait ASTAtom extends Atom
  case class HasType(t: Term, ty: Type) extends ASTAtom
  case class NotHasType(t: Term, ty: Type) extends ASTAtom
  case class Path(src: Term, srcTy: Type, link: Link, trg: Term, trgTy: Type) extends ASTAtom
  case class NoPath(t: Term, ty: Type, link: Link, termIsSource: Boolean) extends ASTAtom
  case class Undef(t: Term) extends ASTAtom

  trait LiteralConst extends Term
  case class IntegerConst(v: Integer) extends LiteralConst
  case class LongConst(v: Long) extends LiteralConst
  case class FloatConst(v: Float) extends LiteralConst
  case class DoubleConst(v: Double) extends LiteralConst
  case class StringLiteral(v: String) extends LiteralConst
  case class BooleanLiteral(v: Boolean) extends LiteralConst
}
