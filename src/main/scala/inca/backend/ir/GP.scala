package inca.backend.ir

object GP {
  sealed trait TypeAnno
  case object TBool extends TypeAnno
  case object TInt extends TypeAnno
  case object TLong extends TypeAnno
  case object TDouble extends TypeAnno
  case object TString extends TypeAnno

  sealed trait TLinked extends TypeAnno
  case object TAnyLinked extends TLinked
  case class TNode(name: String) extends TLinked
  case class TList(contained: TLinked) extends TLinked

  type Name = String

  sealed trait Visibility
  case object Private extends Visibility
  case object Public extends Visibility

  case class Module(name: Name, imports: Seq[Name], pats: Seq[Pattern])
  case class Pattern(vis: Option[Visibility], name: Name, params: Seq[Param], bodies: Seq[Body])
  case class Param(name: Name, typ: Option[TypeAnno])
  case class Body(constraints: Seq[Constraint])

  sealed trait Constraint
  case class Call(name: Name, args: Seq[Term], transitive: Boolean, neg: Boolean) extends Constraint
  case class Compare(comp: Comparator, lhs: Term, rhs: Term) extends Constraint
  case class HasType(t: Term, typ: TypeAnno) extends Constraint
  case class Path(src: Term, trg: Term, link: Link, targetType: TypeAnno) extends Constraint
  case class Computed(resultVar: Var, computation: Computation) extends Constraint

  sealed trait Link
  case object ParentLink extends Link
  case object NextLink extends Link
  case object SizeLink extends Link
  case class NamedLink(node: TNode, field: Name) extends Link

  sealed trait Comparator
  case object EqComparator extends Comparator
  case object NeqComparator extends Comparator

  sealed trait Term
  case class Var(name: Name) extends Term
  case class Constant(lit: Literal) extends Term

  sealed trait Literal
  case class IntLiteral(v: Int) extends Literal
  case class LongLiteral(v: Long) extends Literal
  case class DoubleLiteral(v: Double) extends Literal
  case class StringLiteral(v: String) extends Literal
  case class BooleanLiteral(v: Boolean) extends Literal

  sealed trait Computation
  case class CountAggregation(patName: Name, args: Seq[Term]) extends Computation
  case class Evaluation(freeVars: Iterable[Name], resultType: TypeAnno, code: String) extends Computation
  // TODO
  case class LatticeAggregation() extends Computation
}
