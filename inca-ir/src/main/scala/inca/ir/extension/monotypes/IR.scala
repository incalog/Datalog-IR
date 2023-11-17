package inca.ir.extension.monotypes

import inca.ir.*
import inca.ir.extension.aggregate.{AggregateArg, AggregationOperator}
import inca.ir.extension.arithmetic.TInt
import inca.ir.extension.impure.ImpurityKind
import inca.ir.extension.demand.TDemand

trait IR extends BaseIR:
  override val name: String = "Mono-Types"

  override def language: Language = super.language + IR

  override def requires: Language = Language()

object IR extends IR { }

object MonoImpurityKind extends ImpurityKind:
  override val name: String = "MonoImpurity"
  override val ty: Type = TInt


case class TMono(input: Type, output: Type, keys: Seq[Type]) extends Type:
  override def toString: String = s"Mono[$input, $output]@{${keys.mkString(",")}}"

case class MkMono(mono: MonoDef, args: Seq[Term], keys: Seq[Type]) extends Term:
  override def vars: Seq[Var] = args.flatMap(_.vars)
  override def toString: String = s"new $mono(${args.mkString(", ")})@{${keys.mkString(",")}}"

case class AddMono(m: Term, input: Term, keys: Seq[Term]) extends Atom:
  override def vars: Seq[Var] = m.vars ++ input.vars ++ keys.flatMap{_.vars}
  override def toString: String = s"$m += $input@$keys"

case class ResultMono(m: Term) extends Term:
  override def vars: Seq[Var] = m.vars
  override def toString: String = s"$m.result()"


case class MonoAggregate(rel: Name, args: Seq[AggregateArg], op: MonoDef) extends Term:
  override def vars: Seq[Var] = args.flatMap {
    case AggregateArg.Arg(t) => t.vars
    case AggregateArg.AggregateColumn(t) => t.vars
    case AggregateArg.WildCard(t) => t.vars
  }
  override def toString: String = s"monoAgg($rel(${args.mkString(", ")}), $op)"

trait MonoDef extends AggregationOperator:
  def monotypecheck(constructorArgs: Seq[Term]): Either[String, (Type, Type)]

// RMT: relational mono-type
// NRMT: non-relational mono-type

trait UserDefinedNRMT extends MonoDef

trait UserDefinedRMT extends MonoDef

trait BuiltInNRMT extends MonoDef

trait BuiltInRMT extends MonoDef


enum ArithmeticMono extends BuiltInNRMT:
  case CountMono
  case MaxMono
  case SumMono

  override def monotypecheck(constructorArgs: Seq[Term]): Either[String, (Type, Type)] = this match
    case CountMono | MaxMono | SumMono =>
      if (constructorArgs.nonEmpty)
        Left(s"$CountMono does not take arguments")
      else
        Right((TInt, TInt)) // should be parametric in input type

  override def typecheck(in: Seq[Type]): Either[String, Type] = this match
    case CountMono | MaxMono | SumMono =>
      if (in == Seq(TDemand(TInt)) || in == Seq(TInt))
        Right(TInt)
      else
        Left(s"The input type of $this is not $in")
