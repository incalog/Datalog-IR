package inca.debugger
import inca.backend.ir.Datalog

class IRDebugger extends Debugger {
  override val frontend: DebuggerFrontend = new DebuggerFrontend {
    override def initialize(mod: Datalog.Module): Unit = { }
    override type FrontendPoint = ControlPoint
    override def frontendPoint(cp: ControlPoint): Option[FrontendPoint] = Some(cp)
  }
}