package inca.ir.extension.mono

import inca.ir.*
import inca.ir.extension.arithmetic.{TDouble, TInt}
import inca.ir.extension.block.Block
import inca.ir.extension.demand.TDemand
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.extension.tuple.{TupleLit, TTuple}

trait IR extends BaseIR:
  override val name: String = "Mono"
  override def language: Language = super.language + IR
  override def requires: Language = Language()

object IR extends IR { }

// TODO: also track State type?
case class TMono(input: Type, output: Type, keys: Seq[Type]) extends Type:
  override def toString: String = s"Mono[$input, $output]@{${keys.mkString(",")}}"

case class NewMono(mono: MonoDefinition, keys: Seq[Type], args: Seq[Term]) extends Term:
  override def vars: Seq[Var] = args.flatMap(_.vars)
  override def toString: String = s"new $mono(${args.mkString(", ")})@{${keys.mkString(",")}}"

case class ReadMono(m: Term) extends Term:
  override def vars: Seq[Var] = m.vars
  override def toString: String = s"$m.get"

case class WriteMono(m: Term, input: Term, keys: Seq[Term]) extends Atom:
  override def vars: Seq[Var] = m.vars ++ input.vars ++ keys.flatMap{_.vars}
  override def toString: String = s"$m += $input@{${keys.mkString(",")}}"

case class MonoTypes(in: Type, state: Type, out: Type)

trait MonoDefinition:
  def name: Name
  def constructorParamTypes: Seq[Type]
  def typ: MonoTypes
//  def resultRelation: Relation
  def resultTerm(state: Term): Term
//  =
//    val callAtom = Call(resultRelation.name, Seq(Var(Name("state")), Var(Name("output"))))
//    Block(callAtom, Var(Name("output")))
  def monoType(keys: Seq[Type]): TMono =
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
//  private def createResultRel(body: Body): Relation =
//    Relation(
//      Name(this.name.name ++ "$Result"),
//      Seq(Param(Name("state"), TDemand(typ.state)), Param(Name("output"), typ.out)),
//      Seq(body)
//    )
  override def resultTerm(state: Term): Term = this match
    case MaxInt | MaxDouble | Min | SumInt | SumDouble | Count | CountFrom => state
    case SumToPair => TupleLit(Seq(state, StringLit("this should be a string")))

