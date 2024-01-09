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

/*


Collect_Mono_TBoolean_TSet$TBoolean$$(m: TDemand(Mono_TBoolean_TSet$TBoolean$$), input: TDemand(TBoolean)) {

}

Collect_Mono_TBoolean_TSet$TBoolean$$converted(m: Mono_TBoolean_TSet$TBoolean$$, input: ScalaType(Boolean)) {
  Collect_Mono_TBoolean_TSet$TBoolean$$(m, in),
  input == ConvertIrForeign(in, TBoolean, ScalaType(Boolean))
}

Aggregate_Mono_TBoolean_TSet$TBoolean$$(m: TDemand(Mono_TBoolean_TSet$TBoolean$$), output: TSet[TBoolean]) {
  ?Mono_TBoolean_TSet$TBoolean$$_SetMonoDef_TBoolean(m: <Mono_TBoolean_TSet$TBoolean$$>, id: >TInt<, name: >TString<)
  aggregate(Collect_Mono_TBoolean_TSet$TBoolean$$converted(m: <Mono_TBoolean_TSet$TBoolean$$>, #state: >ScalaType(Set[Boolean])<), MonoAggregationOperator(SetMonoDefinition2(TBoolean,ScalaType(Boolean),ScalaType(Set[Boolean]))))
  output: >TSet[TBoolean]< == state: <ScalaType(Set[Boolean])> as TSet[TBoolean]
}

 */


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


case class StringMonoDefinition() extends BuiltInMonoDefinition:
  override def name: Name = "StringConcatMono"
  override def constructorParamTypes: Seq[Type] = Seq()
  override def typ: MonoTypes = MonoTypes(TString, TString, TString)
  override def resultTerm(state: Term, gensym: Gensym): Term = state

case class DisjMonoDefinition() extends BuiltInMonoDefinition:
  override def name: Name = "DisjMono"
  override def constructorParamTypes: Seq[Type] = Seq()
  override def typ: MonoTypes = MonoTypes(TBoolean, TBoolean, TBoolean)
  override def resultTerm(state: Term, gensym: Gensym): Term = state

/*
 *  T ::= TInt | TBool | TTuple(T,T) | TSet(T)
        | TForeign(FT)
    FT ::= ScalaInt | ScalaBool | ScalaTuple(FT,FT) | ScalaSet(FT)

    [[T]] subseteq RunTime-Values

    [[TInt]] = {...,-1,0,1,...}
    [[TBool]] = {0,1}
    [[TSet(T)]] = relation R where ...
    [[TForeing(ScalaInt)]] = {...,`-1`,`0`,`1`,...}
    [[TForeing(ScalaBool)]] = {`true`, `false`}
    [[TForeign(ScalaTuple(ft1,ft2))]] = [[ft1]] x [[ft2]]
    [[TForeign(ScalaSet(ft))]] = ...
 */

case class SetMonoDefinition(ty: Type) extends BuiltInMonoDefinition:
  override def name: Name = s"SetMonoDef_$ty"
  override def constructorParamTypes: Seq[Type] = Seq()
  override def typ: MonoTypes = MonoTypes(ty, TSet(ty), TSet(ty))
  override def resultTerm(state: Term, gensym: Gensym): Term = state


case class MapMonoDefinition(keyTy: Type, mono: MonoDefinition) extends BuiltInMonoDefinition:
  override def name: Name = s"MapMonoDef_${keyTy}_$mono"
  override def constructorParamTypes: Seq[Type] = Seq()
  override def typ: MonoTypes = MonoTypes(TTuple(Seq(keyTy, mono.typ.in)), TMap(keyTy, mono.typ.state), TMap(keyTy, mono.typ.out))
  override def resultTerm(state: Term, gensym: Gensym): Term = 
    val k = gensym.freshName("k")
    val v = gensym.freshName("v")
    MapFun(
      Seq(Param(k, keyTy)),
      MapLookUp(state, Var(k))
    )
