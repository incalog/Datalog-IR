package inca.ir.extension.mono

import inca.ir.*
import inca.ir.extension.aggregate.{AggregateArg, AggregationOperator}
import inca.ir.extension.arithmetic.TInt
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
  def name: String
  def args: Seq[Type]
  def typ: MonoTypes
  def resultTerm(state: Term): Term
  def monoType(keys: Seq[Type]): TMono =
    val MonoTypes(input, _, output) = typ
    TMono(input, output, keys)

trait BuiltInMonoDefinition extends MonoDefinition
trait UserDefinedMonoDefinition extends MonoDefinition

enum ArithmeticMonoDefinition extends BuiltInMonoDefinition:
  case Max
  case Sum
  case Count
  case CountFrom
  case SumToPair

  override def name: String = this.toString

  override def args: Seq[Type] = this match
    case Count | Max | Sum | SumToPair => Seq()
    case CountFrom => Seq(TInt)

  override def typ: MonoTypes = this match
    case Max | Sum => MonoTypes(TInt, TInt, TInt)
    case Count | CountFrom => MonoTypes(TAny, TInt, TInt)
    case SumToPair => MonoTypes(TInt, TInt, TTuple(Seq(TInt, TString)))

  override def resultTerm(state: Term): Term = this match
    case Max | Sum | Count | CountFrom => state
    case SumToPair => TupleLit(Seq(state, StringLit("This is the sum of aggregands")))
