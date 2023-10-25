package inca.foreign.scala.ir.primitive

import inca.ir.*
import inca.ir.extension.foreign.{ForeignAtom, ForeignLanguage, ForeignModuleEntry, ForeignTerm, ForeignType}
import inca.ir.extension.arithmetic.{TDouble, TInt}
import inca.ir.extension.bool.TBoolean
import inca.ir.extension.data.TData
import inca.ir.extension.string.TString

object ScalaInca extends ForeignLanguage:
  type Code = String

  def compileType(ty: Type): ScalaType = ty match
    case sty@ScalaType(_) => sty
    case TString => ScalaType.string
    case TInt => ScalaType.int
    case TDouble => ScalaType.double
    case TBoolean => ScalaType.bool
    case TData(name) => ScalaType(name)
    case _ => throw IllegalStateException(s"No scala conversion for Type $ty")

case class ScalaType(name: String) extends ForeignType
object ScalaType:
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


enum ScalaAggregation:
  case Min
  case Max
  case Sum
  case Count
  // The ScalaDefnModuleEntry should define an `object` and the name should be the name of the object.
  case Custom(defn: ScalaDefnModuleEntry)

case class ScalaAggregationAtom(agg: ScalaAggregation, rel: Name, out: Term, ty: ScalaType, args: Seq[Term], aggregatedColumn: Int) extends ForeignAtom:
  override def vars: Seq[Var] = args.flatMap(_.vars)

  override def toString: String =
    val inArgs = args.zipWithIndex.map {
      case (_, i) if i == aggregatedColumn => "#"
      case (a, _) => s"$a"
    }
    s"""$out: $ty = aggregate ${rel.name}(${inArgs.mkString(", ")}) with $agg"""


case class ScalaDefnModuleEntry(name: Name, code: String) extends ForeignModuleEntry:
  override def toString: String = code


trait IR extends BaseIR:
  override val name: String = "PrimitiveScala"
  override def language: Language = super.language + new IR {}
  // We can not lower this IR any further
  override def requires: Language = Language(new IR {})
object IR extends IR { }