package inca.ir.extensions

import inca.ir.*

// TODO Discuss: We could make InputGuard a call and InputPattern a relation
//  It did not do this, since we would then require input terms and a body, which we don't care about
enum Demand:
  case Bound
  case Free

// TODO: Is there any case, where we need to store the name of the pattern for the input guard ?
//  It might make our live easier when we compute the full demand transformation, but it should not be require per se
case class InputGuard(demand: Seq[Demand]) extends Atom
case class InputPattern(patternName: Name, params: Seq[Param]) extends ModuleEntry

trait DemandIR extends BaseIR:
  override val name: String = "Data"
  override def language: Language = super.language + new DemandIR {}
  override def requires: Language = Language()