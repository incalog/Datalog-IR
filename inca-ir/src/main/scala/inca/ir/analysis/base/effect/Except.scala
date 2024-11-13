package inca.ir.analysis.base.effect

trait BaseIRException

case class AtomFailed(msg: String) extends BaseIRException

case class BodyFailed(msg: String) extends BaseIRException

case class RelationFailed(msg: String) extends BaseIRException
