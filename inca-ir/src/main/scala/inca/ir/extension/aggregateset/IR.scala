package inca.ir.extension.aggregateset

import inca.ir.*
import inca.ir.extension.aggregate
import inca.ir.extension.aggregate.*

object IR extends IR { }
trait IR extends BaseIR:
  override val name: String = "Arithmetic"
  override def language: Language = super.language + IR
  override def requires: Language = Language(IR) + aggregate.IR

/** Aggregates over a set (rather than a relation) */
case class AggregateSet(rel: Name, args: Seq[AggregateArg], op: AggregationOperator) extends Atom:
  override def toString: String = s"aggregateSet($rel(${args.mkString(", ")}), $op)"
  override def vars: Seq[Var] = args.flatMap {
    case AggregateArg.Arg(t) => t.vars
    case AggregateArg.AggregateColumn(t) => t.vars
  }
