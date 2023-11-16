package inca.foreign.scala.ir.primitive

import inca.ir.*
import inca.ir.extension.foreign.{ForeignAggregationOperator, ForeignAtom, ForeignLanguage, ForeignModuleEntry, ForeignTerm, ForeignType}
import inca.ir.extension.arithmetic.{TDouble, TInt}
import inca.ir.extension.bool.TBoolean
import inca.ir.extension.data.TData
import inca.ir.extension.string.TString
import inca.ir.extension.aggregate.AggregationOperator

object ScalaInca extends ForeignLanguage:
  type Code = String

  def compileType(ty: Type): ScalaType = ty match
    case sty@ScalaType(_) => sty
    case TAny => ScalaType.any
    case TString => ScalaType.string
    case TInt => ScalaType.int
    case TDouble => ScalaType.double
    case TBoolean => ScalaType.bool
    case TData(name) => ScalaType(name)
    case _ => throw IllegalStateException(s"No scala conversion for Type $ty")

case class ScalaType(name: String) extends ForeignType:
  override val lang: ScalaInca.type = ScalaInca
  override val code: String = name

object ScalaType:
  def any: ScalaType = ScalaType("Any")
  def string: ScalaType = ScalaType("String")
  def int: ScalaType = ScalaType("Int")
  def double: ScalaType = ScalaType("Double")
  def bool: ScalaType = ScalaType("Boolean")

case class ScalaTerm(code: String, ty: ScalaType, args: Seq[Term], isApp: Boolean = true) extends ForeignTerm(args):
  override val lang: ScalaInca.type = ScalaInca
  override def vars: Seq[Var] = args.flatMap(_.vars)

  override def inTypes: Seq[ScalaType] = args.map { a =>
    val tty = a.typ match
      case Some(TermType(ty, _)) => ty
      case _ => throw IllegalStateException(s"Untyped argument $a")
    ScalaInca.compileType(tty)
  }
  override def outTypes: Seq[ScalaType] = Seq(ty)
  override def toString: String =
    if (isApp)
      s"""`($code)(${args.mkString(", ")})`"""
    else
      s"""`$code`"""


// Note: We do want to have this type for performance reasons
case class ScalaConstantTerm(code: String, ty: ScalaType) extends ForeignTerm(Seq()):
  override val lang: ScalaInca.type = ScalaInca
  override def vars: Seq[Var] = Seq()
  override def inTypes: Seq[ScalaType] = Seq()
  override def outTypes: Seq[ScalaType] = Seq(ty)
  override def toString: String = s"`$code`"

object ScalaConstantTerm:
  val TRUE: ScalaConstantTerm = ScalaConstantTerm("true", ScalaType.bool)
  val FALSE: ScalaConstantTerm = ScalaConstantTerm("true", ScalaType.bool)


case class ScalaAggregationOperator(ty: ScalaType, code: String) extends ForeignAggregationOperator:
  override val lang: ScalaInca.type = ScalaInca
  def typecheck(in: Seq[Type]): Either[String, Type] = Right(ty)

object ScalaAggregationOperator:
  def Min(ty: ScalaType): ScalaAggregationOperator = ScalaAggregationOperator(ty, s"builtin.arithmetic.Min${ty.name}Aggregation.aggregator")
  def Max(ty: ScalaType): ScalaAggregationOperator = ScalaAggregationOperator(ty, s"builtin.arithmetic.Max${ty.name}Aggregation.aggregator")
  def Sum(ty: ScalaType): ScalaAggregationOperator = ScalaAggregationOperator(ty, s"builtin.arithmetic.Sum${ty.name}Aggregation.aggregator")
  def SumMono: ScalaAggregationOperator = ScalaAggregationOperator(ScalaType.int, s"builtin.arithmetic.SumMono.aggregator")
  def MaxMono: ScalaAggregationOperator = ScalaAggregationOperator(ScalaType.int, s"builtin.arithmetic.MaxMono.aggregator")
  val Count: ScalaAggregationOperator = ScalaAggregationOperator(ScalaType.int, "")
  def Custom(ty: ScalaType, code: String): ScalaAggregationOperator = ScalaAggregationOperator(ty, code)


case class ScalaAggregationAtom(agg: AggregationOperator, rel: Name, out: Term, args: Seq[Term], aggregatedColumn: Int) extends ForeignAtom:
  override val lang: ScalaInca.type = ScalaInca
  override val code: String = agg match
    case ScalaAggregationOperator(_, code) => code
    case _ => "???"

  override def vars: Seq[Var] = args.flatMap(_.vars)

  override def toString: String =
    val inArgs = args.zipWithIndex.map {
      case (_, i) if i == aggregatedColumn => "#"
      case (a, _) => s"$a"
    }
    s"""$out = aggregate ${rel.name}(${inArgs.mkString(", ")}) with $agg"""

case class ScalaDefnModuleEntry(name: Name, code: String) extends ForeignModuleEntry:
  override val lang: ScalaInca.type = ScalaInca
  override def toString: String = code

trait IR extends BaseIR:
  override val name: String = "PrimitiveScala"
  override def language: Language = super.language + new IR {}
  // We can not lower this IR any further
  override def requires: Language = Language(new IR {})
object IR extends IR { }