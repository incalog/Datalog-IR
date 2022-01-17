package inca.debugger

import inca.backend.ir.Datalog

trait DebuggerFrontend {

  def initialize(mod: Datalog.Module): Unit

  type FrontendPoint
  def frontendPoint(cp: ControlPoint): Option[FrontendPoint]

}
