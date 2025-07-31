package inca.ir.analysis.base.effect

import sturdy.values.Finite

trait BaseIRException

case object EmptySupplementary extends BaseIRException

given IRException: Finite[BaseIRException] with {}