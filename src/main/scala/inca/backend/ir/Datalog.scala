package inca.backend.ir

import inca.backend.hints.Hints
import inca.util.Scala
import truechange.{JavaLitType, LitType}

trait Base {
  type Literal
  type BaseType
  type Definition
  type Function
  type Aggregator
}

object ScalaBase extends Base {
  import scala.meta.quasiquotes._

  sealed trait Literal {
    def typ: Datalog.Type
  }
  case class IntLiteral(v: Int) extends Literal {
    override def typ: Datalog.Type = TScalaInt
  }
  case class LongLiteral(v: Long) extends Literal {
    override def typ: Datalog.Type = TScalaLong
  }
  case class DoubleLiteral(v: Double) extends Literal {
    override def typ: Datalog.Type = TScalaDouble
  }
  case class StringLiteral(v: String) extends Literal {
    override def typ: Datalog.Type = TScalaString
  }
  case class BooleanLiteral(v: Boolean) extends Literal {
    override def typ: Datalog.Type = TScalaBoolean
  }
  def True: Datalog.Constant = Datalog.Constant(BooleanLiteral(true))
  def False: Datalog.Constant = Datalog.Constant(BooleanLiteral(false))

  type BaseType = Scala[meta.Type]
  type Definition = Scala[meta.Stat]
  type Function = Scala[meta.Term.Function]
  type Aggregator = Scala[meta.Term]

  def typeAsScala(ty: Datalog.Type): meta.Type = ty match {
    case Datalog.TAny => t"Any"
    case Datalog.TData(name) => meta.Type.Name(name)
    case Datalog.TLiteral(litType) => litType match {
      case JavaLitType(cl) =>  Scala.mkQualTypename(cl.getCanonicalName)
      case _ => throw new UnsupportedOperationException
    }
    case Datalog.TScala(ty) => ty.tree
    case _: Datalog.TLinked => t"truechange.URI"
  }

  object TScalaBoolean extends Datalog.TScala(Scala(t"Boolean"))
  object TScalaInt extends Datalog.TScala(Scala(t"Int"))
  object TScalaLong extends Datalog.TScala(Scala(t"Long"))
  object TScalaDouble extends Datalog.TScala(Scala(t"Double"))
  object TScalaString extends Datalog.TScala(Scala(t"String"))

  def literalFromScalaMeta(t: meta.Lit): Option[Literal] = t match {
    case meta.Lit.Int(i) => Some(IntLiteral(i))
    case meta.Lit.Long(l) => Some(LongLiteral(l))
    case d: meta.Lit.Double => Some(DoubleLiteral(d.value.asInstanceOf[Double]))
    case meta.Lit.Boolean(b) => Some(BooleanLiteral(b))
    case meta.Lit.String(s) => Some(StringLiteral(s))
    case _ => None
  }
}

object Datalog {
  val base: ScalaBase.type = ScalaBase

  type Name = String

  sealed trait Visibility
  case object Private extends Visibility

  case class Module(name: Name, imports: Seq[Name], pats: Seq[Pattern], scalaContent: Seq[base.Definition]) extends Hints {
    override def toString: Name = GPPrinter.prettyModule(this)
    lazy val patternMap: Map[String, Datalog.Pattern] = pats.map { pat => pat.name -> pat }.toMap
  }
  case class Pattern(vis: Option[Visibility], name: Name, params: Seq[Param], bodies: Seq[Body]) extends Hints {
    def isEmpty: Boolean = bodies.isEmpty || bodies.forall(_.atoms.isEmpty)

    /**
     * def R(x) = P(x) union Q(x) union x == 0
     *
     * R(0).
     * R(x) :- P(x).
     * R(y) :- Q(y).
     */
  }
  case class Param(name: Name, typ: Type)

  sealed trait Type extends Hints
  case object TAny extends Type
  case class TData(name: Name) extends Type

  case class TLiteral(litType: LitType) extends Type
  object TLiteral {
    val Bool: TLiteral = TLiteral(JavaLitType(classOf[java.lang.Boolean]))
    val Int: TLiteral = TLiteral(JavaLitType(classOf[java.lang.Integer]))
    val Long: TLiteral = TLiteral(JavaLitType(classOf[java.lang.Long]))
    val Double: TLiteral = TLiteral(JavaLitType(classOf[java.lang.Double]))
    val String: TLiteral = TLiteral(JavaLitType(classOf[java.lang.String]))
  }

  case class TScala(ty: base.BaseType) extends Type

  sealed trait TLinked extends Type
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
  case class Constant(lit: base.Literal) extends Term

  sealed trait Computation {
    val args: Seq[Term]
    def asCall: Option[(Name, Seq[Term])] = None
    def replaceCall(newPatName: Name, newArgs: Seq[Term]): Computation = this
  }
  case class Evaluation(evalArgs: Seq[(Term,Type)], resultType: Type, code: base.Function) extends Computation {
    val args: Seq[Term] = evalArgs.map(_._1)
  }
  case class CountAggregation(patName: Name, args: Seq[Term]) extends Computation {
    override def asCall: Option[(Name, Seq[Term])] = Some(patName -> args)
    override def replaceCall(newPatName: Name, newArgs: Seq[Term]): CountAggregation =
      CountAggregation(newPatName, newArgs)
  }
  case class CustomAggregation(typ: Type, description: Option[String], agg: base.Aggregator, patName: Name, args: Seq[Term], aggregatedColumn: Int) extends Computation {
    override def asCall: Option[(Name, Seq[Term])] = Some(patName -> args)
    override def replaceCall(newPatName: Name, newArgs: Seq[Term]): CustomAggregation =
      CustomAggregation(typ, description, agg, newPatName, newArgs, aggregatedColumn)
  }
}
