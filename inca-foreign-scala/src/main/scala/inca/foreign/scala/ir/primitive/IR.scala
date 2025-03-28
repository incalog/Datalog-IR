package inca.foreign.scala.ir.primitive

import inca.ir.*
import inca.ir.extension.foreign.{ForeignAggregationOperator, ForeignAtom, ForeignLanguage, ForeignModuleEntry, ForeignMonoDefinition, ForeignTerm, ForeignType}
import inca.ir.extension.arithmetic.{TDouble, TInt}
import inca.ir.extension.bool.TBoolean
import inca.ir.extension.data.TData
import inca.ir.extension.string.TString
import inca.ir.extension.mono.{BuiltInMonoDefinition, MonoTypes}
import inca.ir.extension.edbdata.{TEdbList, TEdbNode, TEdbValue}
import inca.ir.extension.map.TMap
import inca.ir.extension.set.TSet
import inca.ir.extension.tuple.TTuple

object ScalaInca extends ForeignLanguage:
  override val name: Name = Name("Scala")
  type Code = String

  def cleanString(name: String): String = name
    .replace(".", "_")
    .replace("@", "__")

  def cleanName(name: Name): Name = Name(cleanString(name.name))

  def compileType(ty: Type): ScalaType = ty match
    case sty@ScalaType(_) => sty
    case TAny => ScalaType.any
    case TNothing => ScalaType.nothing
    case TString => ScalaType.string
    case TInt => ScalaType.int
    case TDouble => ScalaType.double
    case TBoolean => ScalaType.bool
    case TData(ref) => ScalaType(cleanName(ref.name))
    case TSet(sty) => ScalaType(s"Set[${compileType(sty).name}]")
    case TTuple(Seq(ty)) => compileType(ty)
    case TTuple(ty +: tys) => ScalaType(s"(${(ty +: tys).map(compileType.andThen(_.name)).mkString(", ")})")
    case TMap(k, v) => ScalaType(s"Map[${compileType(k).name}, ${compileType(v).name}]")
    case TEdbNode(_) => ScalaType("truechange.URI")
    case TEdbList(ety) => ScalaType("truechange.URI")
    case TEdbValue(ty) => compileType(ty)
    case _ => throw IllegalStateException(s"No scala conversion for Type $ty")

case class ScalaType(name: String) extends ForeignType:
  override val lang: ScalaInca.type = ScalaInca
  override val code: String = name

object ScalaType:
  def any: ScalaType = ScalaType("Any")
  def nothing: ScalaType = ScalaType("Nothing")
  def string: ScalaType = ScalaType("String")
  def int: ScalaType = ScalaType("Int")
  def double: ScalaType = ScalaType("Double")
  def bool: ScalaType = ScalaType("Boolean")

case class ScalaTerm(code: String, ty: Type, args: Seq[Term], isApp: Boolean = true) extends ForeignTerm(args):
  override val lang: ScalaInca.type = ScalaInca

  override def vars: Seq[Var] = args.flatMap(_.vars)

  override def commonVars: Set[Var] = args.flatMap(_.commonVars).toSet

  override def inTypes: Seq[ScalaType] = args.map { a =>
    val tty = a.typ match
      case Some(TermType(ty, _)) => ty
      case _ => throw IllegalStateException(s"Untyped argument $a")
    ScalaInca.compileType(tty)
  }

  override def outTypes: Seq[ScalaType] = Seq(ScalaInca.compileType(ty))

  override def visitArgs(f: Term => Seq[Term]): Seq[Term] =
    Seq(this.copy(args = args.flatMap(f)))

  override def toString: String =
    if (isApp)
      s"""`($code)(${args.mkString(", ")})`"""
    else
      s"""`$code`"""


// Note: We do want to have this type for performance reasons
case class ScalaConstantTerm(code: String, ty: ScalaType) extends ForeignTerm(Seq()):
  override val lang: ScalaInca.type = ScalaInca

  override def vars: Seq[Var] = Seq()

  override def commonVars: Set[Var] = Set()

  override def inTypes: Seq[ScalaType] = Seq()

  override def outTypes: Seq[ScalaType] = Seq(ty)

  override def toString: String = s"`$code`"

  override def visitArgs(f: Term => Seq[Term]): Seq[Term] = Seq(this)

object ScalaConstantTerm:
  val TRUE: ScalaConstantTerm = ScalaConstantTerm("true", ScalaType.bool)
  val FALSE: ScalaConstantTerm = ScalaConstantTerm("false", ScalaType.bool)


case class ScalaAggregationOperator(name: Name, ty: Type, initCode: String, addCode: String) extends ForeignAggregationOperator:
  override val lang: ScalaInca.type = ScalaInca

  override def resultType: Type = ty

  def typecheck(in: Seq[Type]): Option[String] = None

case class ScalaMonoAggregationOperator(name: Name,
                                        stateTy: Type,
                                        inputTy: Type,
                                        outputTy: Type,
                                        initCode: String,
                                        addCode: String,
                                        resultCode: String,
                                        combineCode: String
                                       )
  extends ForeignAggregationOperator:
  override val lang: ScalaInca.type = ScalaInca

  override def resultType: Type = outputTy

  def typecheck(in: Seq[Type]): Option[String] = in match
    case Seq(t) if t == inputTy => None
    case _ => Some(s"Ill-typed mono aggregation, expected $inputTy but got $in")


case class ScalaDefnModuleEntry(name: Name, code: String) extends ForeignModuleEntry:
  def withName(name: String): ScalaDefnModuleEntry = this.copy(name = Name(name))

  override val lang: ScalaInca.type = ScalaInca

  override def toString: String = code

case class ScalaMonoDefinition(name: Name,
                               initCode: String,
                               addCode: String,
                               resultCode: String,
                               combineCode: String,
                               constructorParamTypes: Seq[Type],
                               typ: MonoTypes
                              ) extends ForeignMonoDefinition:
  override val lang: ScalaInca.type = ScalaInca

  def typecheck(in: Seq[Type]): Option[String] = None

  override def toString: String =
    s"""
       |ScalaMono[${typ.in}, ${typ.state}, ${typ.out}]{
       |  def init = $initCode
       |  def add = $addCode
       |  def result = $resultCode
       |}""".stripMargin

object ScalaMonoDefinition:
  def builtinMono(mono: BuiltInMonoDefinition, initCode: String, addCode: String, resultCode: String, combineCode: String): ScalaMonoDefinition =
    ScalaMonoDefinition(mono.name, initCode, addCode, resultCode, combineCode, mono.constructorParamTypes, mono.typ)

trait IR extends BaseIR:
  override val name: String = "PrimitiveScala"

  override def language: Language = super.language + new IR {}

  // We can not lower this IR any further
  override def requires: Language = Language(new IR {})

object IR extends IR {}