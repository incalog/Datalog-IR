package inca.ir.extensions

import inca.ir.*


case class InputGuard(args: Seq[Term]) extends Atom

// TODO: Do we need something like IgnoreCall here ?

trait DemandIR extends BaseIR:
  override val name: String = "Demand"
  override def language: Language = super.language + new DemandIR {}
  override def requires: Language = Language()