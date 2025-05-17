package inca.ir.extension.aggregategeneric

import inca.ir.*
import inca.ir.extension.aggregate
import inca.ir.extension.aggregate.*
import inca.ir.extension.arithmetic

object IR extends IR {}

trait IR extends BaseIR:
  override val name: String = "AggregateGeneric"
  override def language: Language = super.language + IR
  override def requires: Language = Language(IR) + aggregate.IR

// Souffle style aggregate
case class AggregateGeneric(agg: Option[Term], ats: Seq[Atom], op: AggregationOperator) extends Term:
  override def toString: String =
    if (agg.isDefined)
      s"$op ${agg.get} : { ${ats.mkString(", ")} }"
    else
      s"$op : { ${ats.mkString(", ")} }"
  override def vars: Seq[Var] =
    if (agg.isDefined)
      agg.get.vars ++ ats.flatMap(_.vars)
    else
      ats.flatMap(_.vars)
  override def commonVars: Set[Var] =
    if (agg.isDefined)
      agg.get.commonVars ++ ats.flatMap(_.commonVars)
    else
      ats.flatMap(_.commonVars).toSet


object AggregateGeneric:
  def count(ats: Seq[Atom]): AggregateGeneric =
    new AggregateGeneric(None, ats, arithmetic.ArithmeticAggregationOperator.Count)
  def minInt(agg: Term, ats: Seq[Atom]): AggregateGeneric =
    new AggregateGeneric(Some(agg), ats, arithmetic.ArithmeticAggregationOperator.MinInt)
  def maxInt(agg: Term, ats: Seq[Atom]): AggregateGeneric =
    new AggregateGeneric(Some(agg), ats, arithmetic.ArithmeticAggregationOperator.MaxInt)
  def sumInt(agg: Term, ats: Seq[Atom]): AggregateGeneric =
    new AggregateGeneric(Some(agg), ats, arithmetic.ArithmeticAggregationOperator.SumInt)
  def minDouble(agg: Term, ats: Seq[Atom]): AggregateGeneric =
    new AggregateGeneric(Some(agg), ats, arithmetic.ArithmeticAggregationOperator.MinDouble)
  def maxDouble(agg: Term, ats: Seq[Atom]): AggregateGeneric =
    new AggregateGeneric(Some(agg), ats, arithmetic.ArithmeticAggregationOperator.MaxDouble)
  def sumDouble(agg: Term, ats: Seq[Atom]): AggregateGeneric =
    new AggregateGeneric(Some(agg), ats, arithmetic.ArithmeticAggregationOperator.SumDouble)