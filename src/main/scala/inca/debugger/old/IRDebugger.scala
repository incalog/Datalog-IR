package inca.debugger.old

import inca.compiler.CompiledModule

final class IRDebugger(val compiled: CompiledModule) extends Debugger {
  super.initialize(compiled)

  override def stepInto(): Boolean = stepIntoIR()
  override def stepOver(): Boolean = stepOverIR()
  override def stepOut(): Boolean = stepOutIR()

  override type Breakpoint = BreakpointIR
  override def addBreakpoint(bp: Breakpoint): Unit = {
    addBreakpointIR(bp)
  }
  override def removeBreakpoint(bp: Breakpoint): Unit = {
    removeBreakpointIR(bp)
  }
}
