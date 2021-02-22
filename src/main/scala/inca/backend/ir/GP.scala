package inca.backend.ir

import inca.backend.hints.Hints
import inca.util.Meta
import inca.util.Meta.Scala
import truechange.{JavaLitType, LitType}

import scala.meta.quasiquotes._

object GP {
  sealed trait Type {
    def asScala: meta.Type
  }
  case object TAny extends Type {
    override def asScala: meta.Type = t"Any"
  }
  case class TData(name: Name) extends Type {
    override def asScala: meta.Type = meta.Type.Name(name)
  }

  case class TLiteral(litType: LitType) extends Type {
    override def asScala: meta.Type = litType match {
      case JavaLitType(cl) =>  Meta.mkQualTypename(cl.getCanonicalName)
      case _ => throw new UnsupportedOperationException
    }
  }
  object TLiteral {
    val Bool: TLiteral = TLiteral(JavaLitType(classOf[java.lang.Boolean]))
    val Int: TLiteral = TLiteral(JavaLitType(classOf[java.lang.Integer]))
    val Long: TLiteral = TLiteral(JavaLitType(classOf[java.lang.Long]))
    val Double: TLiteral = TLiteral(JavaLitType(classOf[java.lang.Double]))
    val String: TLiteral = TLiteral(JavaLitType(classOf[java.lang.String]))
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

  case class Module(name: Name, imports: Seq[Name], data: Seq[DataDef], pats: Seq[Pattern], scalaContent: Seq[Scala[meta.Stat]]) {
    override def toString: Name = Printer.prettyModule(this)
  }
  case class Pattern(vis: Option[Visibility], name: Name, params: Seq[Param], bodies: Seq[Body]) extends Hints
  case class Param(name: Name, typ: Type)
  case class Body(constraints: Seq[Constraint]) extends Hints

  sealed trait Constraint extends Hints
  case class Call(name: Name, args: Seq[Term], transitive: Boolean = false, neg: Boolean = false) extends Constraint
  case class ExtensionalCall(name: Name, args: Seq[Term], neg: Boolean = false) extends Constraint
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

  def Eq(lhs: Term, rhs: Term): Compare = Compare(EqComparator, lhs, rhs)
  def Neq(lhs: Term, rhs: Term): Compare = Compare(NeqComparator, lhs, rhs)

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
  def True: Constant = Constant(BooleanLiteral(true))
  def False: Constant = Constant(BooleanLiteral(false))

  sealed trait Computation
  case class Evaluation(args: Seq[(Term,Type)], resultType: Type, code: Scala[meta.Term.Function]) extends Computation
  case class CountAggregation(patName: Name, args: Seq[Term]) extends Computation
  case class CustomAggregation(typ: Type, agg: Scala[meta.Term], patName: Name, args: Seq[Term], aggregatedColumn: Int) extends Computation

  case class DataDef(vis: Option[Visibility], name: Name, constrs: Seq[DataConstructor])
  case class DataConstructor(name: Name, paramTypes: Seq[Type])
}
