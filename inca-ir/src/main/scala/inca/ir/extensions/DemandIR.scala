package inca.ir.extensions

import inca.ir.*

enum Demand:
  case Bound
  case Free

// TODO Discuss: If we perform the demand transformation at linking time, then we might not need the demand here.
//  More research required to see if partial information is useful.
case class InputGuard(demand: Seq[Demand]) extends Atom
//case class InputPattern(patternName: Name, params: Seq[Param]) extends ModuleEntry

trait DemandIR extends BaseIR:
  override val name: String = "Data"
  override def language: Language = super.language + new DemandIR {}
  override def requires: Language = Language()