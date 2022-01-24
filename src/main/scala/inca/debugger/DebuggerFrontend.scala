package inca.debugger

import inca.compiler.CompiledModule
import inca.debugger.table.Table

trait DebuggerFrontend {

  type FrontendPoint
  def frontendPoint(cp: ControlPoint): Option[FrontendPoint]

  type FrontendValue
  def frontendTable(fp: FrontendPoint, bound: Table[Value]): Table[FrontendValue]
}
