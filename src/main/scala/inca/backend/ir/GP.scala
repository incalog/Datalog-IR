package inca.backend.ir

import inca.util.Meta.Scala

import scala.meta.quasiquotes._

object GP {
  sealed trait Type {
    def asScala: meta.Type
  }
  case object TAny extends Type {
    override def asScala: meta.Type = t"Any"
  }
  case object TBool extends Type {
    override def asScala: meta.Type = t"Boolean"
  }
  case object TInt extends Type {
    override def asScala: meta.Type = t"Int"
  }
  case object TLong extends Type {
    override def asScala: meta.Type = t"Long"
  }
  case object TDouble extends Type {
    override def asScala: meta.Type = t"Double"
  }
  case object TString extends Type {
    override def asScala: meta.Type = t"String"
  }

  case class TScala(ty: Scala[meta.Type]) extends Type {
    override def asScala: meta.Type = ty.tree
  }
  object TScala {
    def apply(tyString: String): TScala =
      new TScala(Scala(t"Boolean"))
  }
  object TScalaBoolean extends TScala(Scala(t"Boolean"))
  object TScalaInt extends TScala(Scala(t"Int"))
  object TScalaLong extends TScala(Scala(t"Long"))
  object TScalaDouble extends TScala(Scala(t"Double"))
  object TScalaString extends TScala(Scala(t"String"))

  sealed trait TLinked extends Type {
    override def asScala: meta.Type = t"truechange.URI"
  }
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
    override def typ: Type = TScalaInt
  }
  case class LongLiteral(v: Long) extends Literal {
    override def typ: Type = TScalaLong
  }
  case class DoubleLiteral(v: Double) extends Literal {
    override def typ: Type = TScalaDouble
  }
  case class StringLiteral(v: String) extends Literal {
    override def typ: Type = TScalaString
  }
  case class BooleanLiteral(v: Boolean) extends Literal {
    override def typ: Type = TScalaBoolean
  }

  sealed trait Computation
  case class ConstantEvaluation(resultType: Type, code: String) extends Computation
  case class Evaluation(args: Seq[(Term,Type)], resultType: Type, code: meta.Term) extends Computation
  case class CountAggregation(patName: Name, args: Seq[Term]) extends Computation
  case class CustomAggregation(typ: Type, initOp: String, joinOp: String, unjoinOp: Option[String], patName: Name, args: Seq[Term], aggregatedColumn: Int) extends Computation
}
