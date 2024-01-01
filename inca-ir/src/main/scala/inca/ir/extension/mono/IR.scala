package inca.ir.extension.mono

import inca.ir.*
import inca.ir.extension.arithmetic.{TDouble, TInt}
import inca.ir.extension.map.{MapComprehension, MapContains, MapLookUp, TMap}
import inca.ir.extension.set.{SetComprehension, SetMember, TSet}
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.extension.tuple.{TTuple, TupleLit}
import inca.util.Gensym

trait IR extends BaseIR:
  override val name: String = "Mono"
  override def language: Language = super.language + IR
  override def requires: Language = Language()

object IR extends IR { }

// TODO: also track State type?
case class TMono(input: Type, output: Type, keys: Seq[Type]) extends Type:
  override def toString: String =
    val prefix = s"Mono[$input, $output]"
    if keys.nonEmpty then prefix + s"@{${keys.mkString(",")}}" else prefix

// TODO: is it necessary to keep args?
case class NewMono(mono: MonoDefinition, keys: Seq[Type], args: Seq[Term]) extends Term:
  override def vars: Seq[Var] = args.flatMap(_.vars)
  override def toString: String = s"new ${mono.name}(${args.mkString(", ")})@{${keys.mkString(",")}}"

object NewMono:
  def apply(mono: MonoDefinition) : NewMono = NewMono(mono, Seq(), Seq())

case class ReadMono(m: Term) extends Term:
  override def vars: Seq[Var] = m.vars
  override def toString: String = s"$m.get"

case class WriteMono(m: Term, input: Term, keys: Seq[Term]) extends Atom:
  override def vars: Seq[Var] = m.vars ++ input.vars ++ keys.flatMap{_.vars}
  override def toString: String =
    val prefix = s"$m += $input"
    if keys.nonEmpty then prefix + s"@{${keys.mkString(",")}}" else prefix

object WriteMono:
  def apply(m: Term, input: Term): WriteMono = WriteMono(m, input, Seq())

case class MonoTypes(in: Type, state: Type, out: Type)

trait MonoDefinition:
  def name: Name
  def constructorParamTypes: Seq[Type]
  def typ: MonoTypes
  def resultTerm(state: Term, gensym: Gensym): Term
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
  override def resultTerm(state: Term, gensym: Gensym): Term = this match
    case MaxInt | MaxDouble | Min | SumInt | SumDouble | Count | CountFrom => state
    case SumToPair => TupleLit(Seq(state, StringLit("this should be a string")))


object StringMonoDefinition extends BuiltInMonoDefinition:
  override def name: Name = "StringConcatMono"
  override def constructorParamTypes: Seq[Type] = Seq()
  override def typ: MonoTypes = MonoTypes(TString, TString, TString)
  override def resultTerm(state: Term, gensym: Gensym): Term = state

trait SetMonoDefinition(ST: Type, A: Type, B: Type) extends BuiltInMonoDefinition:
  override def name: Name = s"SetMonoDef_${ST}_${A}_$B"
  override def constructorParamTypes: Seq[Type] = Seq()
  override def typ: MonoTypes = MonoTypes(A, TSet(ST), TSet(B))
  def addMap(input: Term): Term
  def resultMap(state: Term): Term
  override def resultTerm(state: Term, gensym: Gensym): Term = 
    val elem = gensym.freshName("elem")
    SetComprehension(
      resultMap(Var(elem)),
      Seq(SetMember(Var(elem), state))
    )

case class NaiveSetMonoDefinition(T: Type) extends SetMonoDefinition(T, T, T):
  override def name: Name = s"NaiveSetMonoDef_$T"
  override def addMap(input: Term): Term = input
  override def resultMap(state: Term): Term = state
  override def resultTerm(state: Term, gensym: Gensym): Term = state

case class MapMonoDefinition(keyTy: Type, mono: MonoDefinition) extends BuiltInMonoDefinition:
  override def name: Name = s"MapMonoDef_${keyTy}_$mono"
  override def constructorParamTypes: Seq[Type] = Seq()
  override def typ: MonoTypes = MonoTypes(TTuple(Seq(keyTy, mono.typ.in)), TMap(keyTy, mono.typ.state), TMap(keyTy, mono.typ.out))
  override def resultTerm(state: Term, gensym: Gensym): Term = 
    val k = gensym.freshName("k")
    val v = gensym.freshName("v")
    MapComprehension(Var(k), mono.resultTerm(Var(v), gensym), Seq(
      MapContains(Var(k), state), 
      Eq(Var(v), MapLookUp(state, Var(k)))
    ))
