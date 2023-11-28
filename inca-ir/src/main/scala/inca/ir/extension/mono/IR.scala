package inca.ir.extension.mono

import inca.ir.*
import inca.ir.extension.aggregate.{AggregateArg, AggregationOperator}
import inca.ir.extension.arithmetic.TInt
import inca.ir.extension.impure.ImpurityKind
import inca.ir.extension.demand.TDemand

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


trait MonoDefinition:
  def typecheckConstructor(args: Seq[Type]): Either[String, (Type, Type)]

trait BuiltInMonoDefinition extends MonoDefinition
trait UserDefinedMonoDefinition extends MonoDefinition


enum ArithmeticMonoDefinition extends BuiltInMonoDefinition:
  case Count
  case Max
  case Sum
  case CountFrom

  override def typecheckConstructor(args: Seq[Type]): Either[String, (Type, Type)] = this match
    case Count =>
      if (args.nonEmpty)
        Left(s"$Count does not take arguments")
      else
        Right((TInt, TInt))  // TODO should be parametric in input type
    case CountFrom => args match
      case Seq(TInt) => Right((TInt, TInt))
      case tys => Left(s"Cannot create $this with $tys arguments")
    case Max | Sum =>
      if (args.nonEmpty)
        Left(s"$this does not take arguments")
      else
        Right((TInt, TInt))
