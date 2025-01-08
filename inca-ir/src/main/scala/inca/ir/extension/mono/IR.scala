package inca.ir.extension.mono

import inca.ir.*
import inca.ir.extension.arithmetic.{TDouble, TInt}
import inca.ir.extension.bool.TBoolean
import inca.ir.extension.map.{MapFun, MapLookUp, TMap}
import inca.ir.extension.set.TSet
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.extension.tuple.{TTuple, TupleLit}
import inca.util.Gensym

trait IR extends BaseIR:
  override val name: String = "Mono"
  override def language: Language = super.language + IR
  override def requires: Language = Language()

object IR extends IR {}

// TODO: also track State type?
case class TMono(input: Type, output: Type, keys: Seq[Type]) extends Type:
  override def toString: String =
    val prefix = s"Mono[$input, $output]"
    if keys.nonEmpty then prefix + s"@{${keys.mkString(",")}}" else prefix

// TODO: is it necessary to keep args?
case class NewMono(mono: MonoDefinition, keys: Seq[Type], args: Seq[Term]) extends Term:
  override def toString: String = s"new ${mono.name}(${args.mkString(", ")})@{${keys.mkString(",")}}"
  override def vars: Seq[Var] = args.flatMap(_.vars)

object NewMono:
  def apply(mono: MonoDefinition): NewMono = NewMono(mono, Seq(), Seq())

case class NewMonoFor(mono: MonoDefinition, keys: Seq[Type], args: Seq[Term], uniqueFor: Seq[Term]) extends Term:
  override def toString: String = s"new ${mono.name}(${args.mkString(", ")}, $uniqueFor)@{${keys.mkString(",")}}"
  override def vars: Seq[Var] = args.flatMap(_.vars)

case class ReadMono(m: Term) extends Term:
  override def toString: String = s"$m.get"
  override def vars: Seq[Var] = m.vars

case class WriteMono(m: Term, input: Term, keys: Seq[Term]) extends Atom:
  override def toString: String =
    val prefix = s"$m += $input"
    if keys.nonEmpty then prefix + s"@{${keys.mkString(",")}}" else prefix
  override def vars: Seq[Var] = m.vars ++ input.vars ++ keys.flatMap(_.vars)

object WriteMono:
  def apply(m: Term, input: Term): WriteMono = WriteMono(m, input, Seq())

case class MonoTypes(in: Type, state: Type, out: Type)

trait MonoDefinition:
  def name: Name
  def constructorParamTypes: Seq[Type]
  def typ: MonoTypes

  final def monoType(keys: Seq[Type]): TMono =
    val MonoTypes(input, _, output) = typ
    TMono(input, output, keys)

trait BuiltInMonoDefinition extends MonoDefinition

trait UserDefinedMonoDefinition extends MonoDefinition

enum ArithmeticMonoDefinition extends BuiltInMonoDefinition:
  case MaxInt
  case MaxDouble
  case Min
  case SumInt
  case SumDouble
  case Count
  case CountFrom
  case SumToPair

  override def name: Name = Name(this.toString)

  override def constructorParamTypes: Seq[Type] = this match
    case CountFrom | Min => Seq(TInt)
    case _ => Seq()

  override def typ: MonoTypes = this match
    case MaxInt | SumInt | Min => MonoTypes(TInt, TInt, TInt)
    case Count | CountFrom => MonoTypes(TAny, TInt, TInt)
    case SumDouble | MaxDouble => MonoTypes(TDouble, TDouble, TDouble)
    case SumToPair => MonoTypes(TInt, TInt, TTuple(Seq(TInt, TString)))


case class StringConcatMonoDefinition() extends BuiltInMonoDefinition:
  override def name: Name = "StringConcatMono"
  override def constructorParamTypes: Seq[Type] = Seq()
  override def typ: MonoTypes = MonoTypes(TString, TString, TString)

case class DisjMonoDefinition() extends BuiltInMonoDefinition:
  override def name: Name = "DisjMono"
  override def constructorParamTypes: Seq[Type] = Seq()
  override def typ: MonoTypes = MonoTypes(TBoolean, TBoolean, TBoolean)

case class SetMonoDefinition(ty: Type) extends BuiltInMonoDefinition:
  override def name: Name = s"SetMonoDef_$ty"
  override def constructorParamTypes: Seq[Type] = Seq()
  override def typ: MonoTypes = MonoTypes(ty, TSet(ty), TSet(ty))

case class MapMonoDefinition(keyTy: Type, mono: MonoDefinition) extends BuiltInMonoDefinition:
  override def name: Name = s"MapMonoDef_${keyTy}_${mono.name}"
  override def constructorParamTypes: Seq[Type] = Seq()
  override def typ: MonoTypes = MonoTypes(TTuple(Seq(keyTy, mono.typ.in)), TMap(keyTy, mono.typ.state), TMap(keyTy, mono.typ.out))
