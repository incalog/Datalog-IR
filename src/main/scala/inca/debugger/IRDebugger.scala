package inca.debugger

import inca.compiler.CompiledModule

final class IRDebugger(val compiled: CompiledModule) extends Debugger {
  super.initialize(compiled)

  override def stepInto(): Unit = stepIntoIR()
  override def stepOver(): Unit = stepOverIR()
  override def stepOut(): Unit = stepOutIR()

  // TODO
  override type Breakpoint = Any
  override def addBreakpoint(bp: Breakpoint): Unit = {}
  override def removeBreakpoint(bp: Breakpoint): Unit = {}
}
