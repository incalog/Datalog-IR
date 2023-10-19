package inca.ir.extension.aggregate

import inca.ir.{Atom, BaseIR, Language, Name, Term, Type, Var}

object IR extends IR { }
trait IR extends BaseIR:
  override val name: String = "Aggregate"
  override def language: Language = super.language + IR
  override def requires: Language = Language(IR)

case class Aggregate(rel: Name, args: Seq[AggregateArg], op: AggregationOperator) extends Atom:
  override def toString: String = s"aggregate($rel(${args.mkString(", ")}), $op)"
  override def vars: Seq[Var] = args.flatMap {
    case AggregateArg.Arg(t) => t.vars
    case AggregateArg.AggregateColumn(t) => t.vars
  }



enum AggregateArg:
  case Arg(t: Term)
  case AggregateColumn(t: Term)

  override def toString: String = this match
    case Arg(t) => t.toString
    case AggregateColumn(t) => s"#$t"

trait AggregationOperator:
  def typecheck(in: Seq[Type]): Either[String, Type]

trait AggregationOperatorBuiltIn extends AggregationOperator
trait AggregationOperatorUserDefined extends AggregationOperator
