package inca.debugger.redesign

import inca.backend.ir.Datalog

final class IRDebugger(val module: Datalog.Module) extends Debugger(module) {

  override def stepInto(): Unit = stepIntoIR()
  override def stepOver(): Unit = stepOverIR()
  override def stepOut(): Unit = ???

  override type Breakpoint = IRBreakpoint
  override def addBreakpoint(bp: Breakpoint): Unit = breakpointHandler.addBreakpoint(bp)
  override def removeBreakpoint(bp: Breakpoint): Unit = breakpointHandler.removeBreakpoint(bp)
  override def clearBreakpoints(): Unit = breakpointHandler.clearBreakpoints()
}
