package inca.ir.extensions

import inca.Scala
import inca.ir.*
import inca.ir.extension.primitiveScala.IR

case class Aggregation(out: Term, fun: Scala.Term, patternName: Name, args: Seq[Term], aggregatedColumn: Int)

// We probably want this because viatra has built-in support for count aggregations
case class CountAggregation(out: Term, patternName: Name, args: Seq[Term])

trait AggregationScalaIR extends BaseIR:
  override val name: String = "AggregationScala"
  override def language: Language = super.language + new AggregationScalaIR {}
  override def requires: Language = Language(new IR {})