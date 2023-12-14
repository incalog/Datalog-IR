package inca.foreign.scala.ir.primitive

import inca.ir.*
import inca.ir.extension.foreign.{ForeignAggregationOperator, ForeignAtom, ForeignLanguage, ForeignModuleEntry, ForeignMonoDefinition, ForeignTerm, ForeignType}
import inca.ir.extension.arithmetic.{TDouble, TInt}
import inca.ir.extension.bool.TBoolean
import inca.ir.extension.data.TData
import inca.ir.extension.string.TString
import inca.ir.extension.aggregate.AggregationOperator
import inca.ir.extension.mono.{BuiltInMonoDefinition, MonoDefinition, MonoTypes, ArithmeticMonoDefinition as ArithMonoDef}
import inca.ir.extension.block.Block
import inca.ir.extension.set.TSet
import inca.ir.visitors.BaseIRVisitor
import inca.foreign.scala.visitors.ScalaVisitor
import inca.ir.extension.set.TSet
import inca.ir.extension.tuple.TTuple

object ScalaInca extends ForeignLanguage:
  type Code = String

  def compileType(ty: Type): ScalaType = ty match
    case sty@ScalaType(_) => sty
    case TAny => ScalaType.any
    case TString => ScalaType.string
    case TInt => ScalaType.int
    case TDouble => ScalaType.double
    case TBoolean => ScalaType.bool
    case TData(RefByName(name)) => ScalaType(name)
    case TSet(ty) => ScalaType(s"Set[${compileType(ty).name}]")
    case TTuple(tys) => ScalaType(s"(${tys.map(compileType.andThen(_.name))})")
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
  override def visitor: BaseIRVisitor = new ScalaVisitor {}
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
  override def visitor: BaseIRVisitor = new ScalaVisitor {}

object ScalaConstantTerm:
  val TRUE: ScalaConstantTerm = ScalaConstantTerm("true", ScalaType.bool)
  val FALSE: ScalaConstantTerm = ScalaConstantTerm("true", ScalaType.bool)



case class ScalaAggregationOperator(name: Name, ty: ScalaType, initCode: String, addCode: String) extends ForeignAggregationOperator:
  override val lang: ScalaInca.type = ScalaInca
  override def resultType: Type = ty
  def typecheck(in: Seq[Type]): Option[String] = None

case class ScalaMonoAggregationOperator(name: Name,
                                        inputTy: ScalaType,
                                        stateTy: ScalaType,
                                        initCode: String,
                                        addCode: String)
  extends ForeignAggregationOperator:
  override val lang: ScalaInca.type = ScalaInca
  override def resultType: Type = stateTy
  def typecheck(in: Seq[Type]): Option[String] = None


case class ScalaAggregationAtom(op: AggregationOperator, rel: Name, out: Term, args: Seq[Term], aggregatedColumn: Int) extends ForeignAtom:
  override val lang: ScalaInca.type = ScalaInca
  override val code: String = op match
    case ScalaAggregationOperator(_, _, initCode, addCode) => s"" // TODO: code
    case _ => "???"

  override def vars: Seq[Var] = args.flatMap(_.vars)

  override def toString: String =
    val inArgs = args.zipWithIndex.map {
      case (_, i) if i == aggregatedColumn => "#"
      case (a, _) => s"$a"
    }
    s"""$out = aggregate ${rel.name}(${inArgs.mkString(", ")}) with $op"""

case class ScalaDefnModuleEntry(name: Name, code: String) extends ForeignModuleEntry:
  def withExtendedName(suffix: String): ScalaDefnModuleEntry = this.copy(name = Name(name.name + suffix))
  override val lang: ScalaInca.type = ScalaInca
  override def toString: String = code

case class ScalaMonoDefinition(name: Name,
                               initCode: String,
                               addCode: String,
                               resultCode: String,
                               constructorParamTypes: Seq[Type],
                               typ: MonoTypes) extends ForeignMonoDefinition:
  override val lang: ScalaInca.type = ScalaInca
  def typecheck(in: Seq[Type]): Option[String] = None
  override def resultTerm(state: Term): Term =
    ScalaTerm(resultCode, ScalaInca.compileType(typ.out), Seq(state))

  override def toString: String =
    s"""
       |ScalaMono[${typ.in}, ${typ.state}, ${typ.out}]{
       |  def init = $initCode
       |  def add = $addCode
       |  def result = $resultCode
       |}""".stripMargin

object ScalaMonoDefinition:
  def builtinMono(mono: BuiltInMonoDefinition, initCode: String, addCode: String, resultCode: String): ScalaMonoDefinition =
    ScalaMonoDefinition(mono.name, initCode, addCode, resultCode, mono.constructorParamTypes, mono.typ)

trait IR extends BaseIR:
  override val name: String = "PrimitiveScala"
  override def language: Language = super.language + new IR {}
  // We can not lower this IR any further
  override def requires: Language = Language(new IR {})
object IR extends IR { }