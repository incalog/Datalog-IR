package inca.ir.extensions

import inca.ir.*


case class InputGuard(args: Seq[Term]) extends Atom

trait DemandIR extends BaseIR:
  override val name: String = "Demand"
  override def language: Language = super.language + new DemandIR {}
  override def requires: Language = Language()