package inca.backend.ir

import inca.util.Meta.Scala

object GP {
  sealed trait Type
  case class TUnbounded(ty: Type) extends Type
  case object TAny extends Type
  case object TBool extends Type
  case object TInt extends Type
  case object TLong extends Type
  case object TDouble extends Type
  case object TString extends Type

  case class TDataType(qname: String) extends Type

  sealed trait TLinked extends Type
  case object TAnyLinked extends TLinked
  case class TNode(name: String) extends TLinked
  case class TList(contained: TLinked) extends TLinked

  type Name = String

  sealed trait Visibility
  case object Private extends Visibility
  case object Public extends Visibility

  case class Module(name: Name, imports: Seq[Name], pats: Seq[Pattern], stats: Seq[Scala[meta.Stat]]) {
    override def toString: Name = Printer.prettyModule(this)
  }
  case class Pattern(vis: Option[Visibility], name: Name, params: Seq[Param], bodies: Seq[Body])
  case class Param(name: Name, typ: Type)
  case class Body(constraints: Seq[Constraint])

  sealed trait Constraint
  case class Call(name: Name, args: Seq[Term], transitive: Boolean, neg: Boolean) extends Constraint
  case class Compare(comp: Comparator, lhs: Term, rhs: Term) extends Constraint
  case class HasType(t: Term, typ: Type) extends Constraint
  case class NotHasType(t: Term, typ: Type) extends Constraint
  case class Path(src: Term, srcTy: Type, link: Link, trg: Term, trgTy: Type) extends Constraint
  case class NoPath(t: Term, ty: Type, link: Link, termIsSource: Boolean) extends Constraint
  case class Computed(lhs: Term, computation: Computation) extends Constraint

  sealed trait Link
  case object ParentLink extends Link
  case object NextLink extends Link
  case object SizeLink extends Link
  case class NamedLink(node: TNode, field: Name) extends Link

  sealed trait Comparator
  case object EqComparator extends Comparator
  case object NeqComparator extends Comparator

  sealed trait Term
  case class Var(name: Name) extends Term {
    private[backend] var typ: Option[Type] = None
  }
  case class Constant(lit: Literal) extends Term

  sealed trait Literal {
    def typ: Type
  }
  case class IntLiteral(v: Int) extends Literal {
    override def typ: Type = TInt
  }
  case class LongLiteral(v: Long) extends Literal {
    override def typ: Type = TLong
  }
  case class DoubleLiteral(v: Double) extends Literal {
    override def typ: Type = TDouble
  }
  case class StringLiteral(v: String) extends Literal {
    override def typ: Type = TString
  }
  case class BooleanLiteral(v: Boolean) extends Literal {
    override def typ: Type = TBool
  }

  sealed trait Computation
  case class ConstantEvaluation(resultType: Type, code: String) extends Computation
  case class Evaluation(args: Seq[(Term,Type)], resultType: Type, code: meta.Term) extends Computation
  case class CountAggregation(patName: Name, args: Seq[Term]) extends Computation
  case class CustomAggregation(typ: Type, initOp: String, joinOp: String, unjoinOp: Option[String], patName: Name, args: Seq[Term], aggregatedColumn: Int) extends Computation
}
