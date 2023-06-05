package inca.ir.extensions

import inca.Scala
import inca.ir.*

case class Aggregation(out: Term, fun: Scala.Fun, patternName: Name, args: Seq[Term], aggregatedColumn: Int)

// We probably want this because viatra has built-in support
// case class CountAggregation(out: Term, patternName: Name, args: Seq[Term])

trait AggregationIR extends BaseIR:
  override val name: String = "Aggregation"
  override def language: Language = super.language + new AggregationIR {}
  override def requires: Language = Language(new PrimitiveIR {})