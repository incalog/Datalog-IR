package inca.ir.analysis.base.effect

import sturdy.values.Finite

trait BaseIRException

case class AtomFailed(msg: String) extends BaseIRException

case class RelationFailed(msg: String) extends BaseIRException

case class MergeFailed(msg: String) extends BaseIRException


given IRException: Finite[BaseIRException] with {}