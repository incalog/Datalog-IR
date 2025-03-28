package inca.ir.extension.aggregateset

import inca.ir.*
import inca.ir.extension.aggregate
import inca.ir.extension.aggregate.*

object IR extends IR {}

trait IR extends BaseIR:
  override val name: String = "AggregateSet"

  override def language: Language = super.language + IR

  override def requires: Language = Language(IR) + aggregate.IR

/** Aggregates over a set (rather than a relation) */
case class AggregateSet(rel: Ref[Relation], args: Seq[Arg], op: AggregationOperator) extends Atom:
  override def toString: String = s"aggregateSet($rel(${args.mkString(", ")}), $op)"
  override def vars: Seq[Var] = args.flatMap(_.vars)
  override def commonVars: Set[Var] = args.flatMap(_.commonVars).toSet
