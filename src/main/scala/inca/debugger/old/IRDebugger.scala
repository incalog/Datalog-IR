package inca.debugger.old

import inca.compiler.CompiledModule

final class IRDebugger(val compiled: CompiledModule) extends Debugger {
  super.initialize(compiled)

  override def stepInto(): Unit = stepIntoIR()
  override def stepOver(): Unit = stepOverIR()
  override def stepOut(): Unit = stepOutIR()

  override type Breakpoint = BreakpointIR
  override def addBreakpoint(bp: Breakpoint): Unit = {
    addBreakpointIR(bp)
  }
  override def removeBreakpoint(bp: Breakpoint): Unit = {
    removeBreakpointIR(bp)
  }
}
