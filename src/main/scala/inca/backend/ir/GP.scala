package inca.backend.ir

object GP {
  sealed trait TypeAnno
  case class TUnbounded(ty: TypeAnno) extends TypeAnno
  case object TAny extends TypeAnno
  case object TBool extends TypeAnno
  case object TInt extends TypeAnno
  case object TLong extends TypeAnno
  case object TDouble extends TypeAnno
  case object TString extends TypeAnno

  case class TDataType(qname: String) extends TypeAnno

  sealed trait TLinked extends TypeAnno
  case object TAnyLinked extends TLinked
  case class TNode(name: String) extends TLinked
  case class TList(contained: TLinked) extends TLinked

  type Name = String

  sealed trait Visibility
  case object Private extends Visibility
  case object Public extends Visibility

  case class Module(name: Name, imports: Seq[Name], pats: Seq[Pattern]) {
    override def toString: Name = Printer.prettyModule(this)
  }
  case class Pattern(vis: Option[Visibility], name: Name, params: Seq[Param], bodies: Seq[Body])
  case class Param(name: Name, typ: TypeAnno)
  case class Body(constraints: Seq[Constraint])

  sealed trait Constraint
  case class Call(name: Name, args: Seq[Term], transitive: Boolean, neg: Boolean) extends Constraint
  case class Compare(comp: Comparator, lhs: Term, rhs: Term) extends Constraint
  case class HasType(t: Term, typ: TypeAnno) extends Constraint
  case class NotHasType(t: Term, typ: TypeAnno) extends Constraint
  case class Path(src: Term, srcTy: TypeAnno, link: Link, trg: Term, trgTy: TypeAnno) extends Constraint
  case class NoPath(t: Term, ty: TypeAnno, link: Link, termIsSource: Boolean) extends Constraint
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
    private[backend] var typ: Option[TypeAnno] = None
  }
  case class Constant(lit: Literal) extends Term

  sealed trait Literal {
    def typ: TypeAnno
  }
  case class IntLiteral(v: Int) extends Literal {
    override def typ: TypeAnno = TInt
  }
  case class LongLiteral(v: Long) extends Literal {
    override def typ: TypeAnno = TLong
  }
  case class DoubleLiteral(v: Double) extends Literal {
    override def typ: TypeAnno = TDouble
  }
  case class StringLiteral(v: String) extends Literal {
    override def typ: TypeAnno = TString
  }
  case class BooleanLiteral(v: Boolean) extends Literal {
    override def typ: TypeAnno = TBool
  }

  sealed trait Computation
  case class Evaluation(args: Seq[(Term,TypeAnno)], resultType: TypeAnno, code: String) extends Computation
  case class CountAggregation(patName: Name, args: Seq[Term]) extends Computation
  case class CustomAggregation(typ: TypeAnno, initOp: String, joinOp: String, unjoinOp: Option[String], patName: Name, args: Seq[Term], aggregatedColumn: Int) extends Computation
}
