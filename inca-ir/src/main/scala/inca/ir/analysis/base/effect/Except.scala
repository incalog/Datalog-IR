package inca.ir.analysis.base.effect

import sturdy.values.Finite

trait BaseIRException

case object EmptySupplementary extends BaseIRException

case class AtomFailed(msg: String) extends BaseIRException

given IRException: Finite[BaseIRException] with {}