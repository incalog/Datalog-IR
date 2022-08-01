package inca.backend.ir

import inca.backend.hints.Hints
import truechange.JavaLitType
import truechange.LitType

object DatalogScala extends Datalog {
  val host: ScalaHost.type = ScalaHost

  val collectVarNames: CollectVarNames[DatalogScala.type] = new CollectVarNames[DatalogScala.type] {
    override val datalog: DatalogScala.type = DatalogScala
  }
  val collectVars: CollectVars[DatalogScala.type] = new CollectVars[DatalogScala.type] {
    override val datalog: DatalogScala.type = DatalogScala
  }
  val collectLits: CollectLits[DatalogScala.type] = new CollectLits[DatalogScala.type] {
    override val datalog: DatalogScala.type = DatalogScala
  }
  val collectConstantEvaluation: CollectConstantEvaluation[DatalogScala.type] = new CollectConstantEvaluation[DatalogScala.type] {
    override val datalog: DatalogScala.type = DatalogScala
  }
}

trait Datalog {
  val host: ScalaHost.type

  private val printer = new DatalogPrinter[this.type](this)

  type Name = String

  sealed trait Visibility
  case object Private extends Visibility

  case class Module(
      name: Name,
      imports: Seq[Name],
      pats: Seq[Pattern],
      scalaContent: Seq[host.Definition])
      extends Hints {
    override def toString: Name = printer.prettyModule(Module(name, imports, pats, scalaContent))
    lazy val patternMap: Map[String, Pattern] = pats.map { pat => pat.name -> pat }.toMap
  }

  case class Pattern(vis: Option[Visibility], name: Name, params: Seq[Param], bodies: Seq[Body])
      extends Hints {
    def isEmpty: Boolean = bodies.isEmpty || bodies.forall(_.atoms.isEmpty)
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

  case class TScala(ty: host.BaseType) extends Type

  sealed trait TLinked extends Type
  case object TAnyLinked extends TLinked
  case class TNode(name: String) extends TLinked
  case class TList(contained: TLinked) extends TLinked

  case class Body(atoms: Seq[Atom]) extends Hints

  sealed trait Atom extends Hints {
    def asCall: Option[(Name, Seq[Term])] = None
    def replaceCall(newPatName: Name, newArgs: Seq[Term]): Atom = this
  }
  case class Call(name: Name, args: Seq[Term], transitive: Boolean = false, neg: Boolean = false)
      extends Atom {
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
  case class Constant(lit: host.Literal) extends Term

  sealed trait Computation {
    val args: Seq[Term]
    def asCall: Option[(Name, Seq[Term])] = None
    def replaceCall(newPatName: Name, newArgs: Seq[Term]): Computation = this
  }
  case class Evaluation(
      evalArgs: Seq[(Term, Type)],
      resultType: Type,
      code: host.Function)
      extends Computation {
    val args: Seq[Term] = evalArgs.map(_._1)
  }
  case class CountAggregation(patName: Name, args: Seq[Term]) extends Computation {
    override def asCall: Option[(Name, Seq[Term])] = Some(patName -> args)
    override def replaceCall(newPatName: Name, newArgs: Seq[Term]): CountAggregation =
      CountAggregation(newPatName, newArgs)
  }
  case class CustomAggregation(
                                typ: Type,
                                description: Option[String],
                                agg: host.Aggregator,
                                patName: Name,
                                args: Seq[Term],
                                aggregatedColumn: Int)
      extends Computation {
    override def asCall: Option[(Name, Seq[Term])] = Some(patName -> args)
    override def replaceCall(newPatName: Name, newArgs: Seq[Term]): CustomAggregation =
      CustomAggregation(typ, description, agg, newPatName, newArgs, aggregatedColumn)
  }
}
