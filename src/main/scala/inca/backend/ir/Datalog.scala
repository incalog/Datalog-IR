package inca.backend.ir

import inca.backend.hints.{Hints, OptimizationHints}
import inca.backend.ir.util.printer.GPPrinter
import inca.util.Scala
import truechange.{JavaLitType, LitType}

import scala.meta.XtensionQuasiquoteType

object Datalog {
  case object BodyMustFail extends java.lang.Exception
  def throwBodyMustFail(): Nothing = throw BodyMustFail

  type Name = String

  sealed trait Visibility
  case object Private extends Visibility

  case class Module(name: Name, imports: Seq[Name], pats: Seq[Pattern], scalaContent: Seq[Scala[meta.Stat]]) {
    override def toString: Name = GPPrinter.prettyModule(this)
  }
  case class Pattern(vis: Option[Visibility], name: Name, params: Seq[Param], bodies: Seq[Body]) extends Hints {
    def isEmpty: Boolean = bodies.isEmpty || bodies.forall(_.atoms.isEmpty)
  }
  case class Param(name: Name, typ: Type)

  sealed trait Type extends Hints {
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
      case JavaLitType(cl) =>  Scala.mkQualTypename(cl.getCanonicalName)
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
    def apply(tyString: String): TScala = tyString match {
      case "Boolean" => TScalaBoolean
      case "Int" => TScalaInt
      case "Long" => TScalaLong
      case "Double" => TScalaDouble
      case "String" => TScalaString
    }
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

  case class Body(atoms: Seq[Atom]) extends Hints

  sealed trait Atom extends Hints {
    def asCall: Option[(Name, Seq[Term])] = None
    def replaceCall(newPatName: Name, newArgs: Seq[Term]): Atom = this
  }
  case class Call(name: Name, args: Seq[Term], transitive: Boolean = false, neg: Boolean = false) extends Atom {
    override def asCall: Option[(Name, Seq[Term])] = Some(name -> args)
    override def replaceCall(newPatName: Name, newArgs: Seq[Term]): Call =
      Call(newPatName, newArgs, transitive, neg).withHints(this)
  }
  case class ExtensionalCall(name: Name, args: Seq[Term], neg: Boolean = false) extends Atom

  case class Compare(comp: Comparator, lhs: Term, rhs: Term) extends Atom
  def Eq(lhs: Term, rhs: Term): Compare = Compare(EqComparator, lhs, rhs)
  def Neq(lhs: Term, rhs: Term): Compare = Compare(NeqComparator, lhs, rhs)

  case class HasType(t: Term, typ: Type) extends Atom
  case class NotHasType(t: Term, typ: Type) extends Atom

  case class Path(src: Term, srcTy: Type, link: Link, trg: Term, trgTy: Type) extends Atom
  case class NoPath(t: Term, ty: Type, link: Link, termIsSource: Boolean) extends Atom

  case class Undef(t: Term) extends Atom

  case class Computed(lhs: Term, computation: Computation) extends Atom {
    override def asCall: Option[(Name, Seq[Term])] = computation.asCall
    override def replaceCall(newPatName: Name, newArgs: Seq[Term]): Computed =
      Computed(lhs, computation.replaceCall(newPatName, newArgs)).withHints(this)
  }

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
  object Literal {
    def fromScalaMeta(t: meta.Lit): Option[Literal] = t match {
      case meta.Lit.Int(i) => Some(IntLiteral(i))
      case meta.Lit.Long(l) => Some(LongLiteral(l))
      case d: meta.Lit.Double => Some(DoubleLiteral(d.value.asInstanceOf[Double]))
      case meta.Lit.Boolean(b) => Some(BooleanLiteral(b))
      case meta.Lit.String(s) => Some(StringLiteral(s))
      case _ => None
    }
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
  def StringConstant(v: String): Constant = Constant(StringLiteral(v))
  def IntConstant(v: Int): Constant = Constant(IntLiteral(v))
  def LongConstant(v: Long): Constant = Constant(LongLiteral(v))
  def DoubleConstant(v: Double): Constant = Constant(DoubleLiteral(v))

  sealed trait Computation {
    val args: Seq[Term]
    def asCall: Option[(Name, Seq[Term])] = None
    def replaceCall(newPatName: Name, newArgs: Seq[Term]): Computation = this
  }
  case class Evaluation(evalArgs: Seq[(Term,Type)], resultType: Type, code: Scala[meta.Term.Function]) extends Computation {
    val args: Seq[Term] = evalArgs.map(_._1)
  }
  case class CountAggregation(patName: Name, args: Seq[Term]) extends Computation {
    override def asCall: Option[(Name, Seq[Term])] = Some(patName -> args)
    override def replaceCall(newPatName: Name, newArgs: Seq[Term]): CountAggregation =
      CountAggregation(newPatName, newArgs)
  }
  case class CustomAggregation(typ: Type, description: Option[String], agg: Scala[meta.Term], patName: Name, args: Seq[Term], aggregatedColumn: Int) extends Computation {
    override def asCall: Option[(Name, Seq[Term])] = Some(patName -> args)
    override def replaceCall(newPatName: Name, newArgs: Seq[Term]): CustomAggregation =
      CustomAggregation(typ, description, agg, newPatName, newArgs, aggregatedColumn)
  }
}
