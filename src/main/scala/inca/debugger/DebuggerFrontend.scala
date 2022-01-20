package inca.debugger

import inca.backend.ir.Datalog
import inca.debugger.table.Table

trait DebuggerFrontend {

  def initialize(mod: Datalog.Module): Unit

  type FrontendPoint
  def frontendPoint(cp: ControlPoint): Option[FrontendPoint]

  type FrontendValue
  def frontendTable(fp: FrontendPoint, bound: Table[Value]): Table[FrontendValue]
}
