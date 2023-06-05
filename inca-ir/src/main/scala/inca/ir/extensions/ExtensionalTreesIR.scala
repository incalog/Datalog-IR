package inca.ir.extensions

import inca.ir.*



trait ExtensionalTreesIR extends BaseIR:
  override val name: String = "ExtensionalTrees"
  override def language: Language = super.language + new AggregationIR {}
  override def requires: Language = Language()
