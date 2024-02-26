package inca.ir.extension.mono

import inca.ir.Type
import inca.ir.extension.aggregate.AggregationOperatorUserDefined
import inca.ir.extension.demand.TDemand


case class MonoAggregationOperator(mono: MonoDefinition) extends AggregationOperatorUserDefined:
  override def resultType: Type = mono.typ.out
  override def typecheck(in: Seq[Type]): Option[String] = in match
    case Seq(input) if input == mono.typ._1 => None
    case Seq(TDemand(input)) if input == mono.typ._1 => None
    case _ => Some(s"Ill-typed mono aggregation, expected ${mono.typ._1} but got $in")
