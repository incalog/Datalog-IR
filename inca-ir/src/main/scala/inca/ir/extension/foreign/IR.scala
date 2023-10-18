package inca.ir.extension.foreign

import inca.ir.extension.aggregate.AggregationOperatorUserDefined
import inca.ir.{Term, Type, Var}

trait ForeignLanguage:
  type Code

trait ForeignTerm(args: Seq[Term]) extends Term:
  val lang: ForeignLanguage
  val code: lang.Code
  
  def inTypes: Seq[Type]
  def outTypes: Seq[Type]
  
  override def vars: Seq[Var] = args.flatMap(_.vars)


trait ForeignAggregationOperator extends AggregationOperatorUserDefined:
  val lang: ForeignLanguage
  val code: lang.Code
