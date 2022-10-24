package inca.debugger.redesign

import inca.backend.ir.Datalog
import inca.compiler.CompiledDatalogModule
import inca.runtime.db.DatabaseInput

final class IRDebugger(module: CompiledDatalogModule, input: DatabaseInput) extends Debugger {

  this.initialize(module)
  this.initializeDatabaseRuntime(input)

  override def doStepInto(): Boolean = stepIntoIR()
  override def doStepOver(): Boolean = stepOverIR()
  override def doStepOut(): Boolean = stepOutIR()

  override type Breakpoint = IRBreakpoint
  override def addBreakpoint(bp: Breakpoint): Unit = breakpointHandler.addBreakpoint(bp)
  override def removeBreakpoint(bp: Breakpoint): Unit = breakpointHandler.removeBreakpoint(bp)
}
