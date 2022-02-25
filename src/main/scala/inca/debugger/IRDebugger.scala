package inca.debugger
import inca.compiler.CompiledModule

final class IRDebugger(compiled: CompiledModule) extends Debugger {
  super.initialize(compiled)

  override def stepInto(): Unit = stepIntoIR()
  override def stepOver(): Unit = stepOverIR()
  override def stepOut(): Unit = ???

  override type Breakpoint = Nothing
  override def addBreakpoint(bp: Breakpoint): Unit = ???
  override def removeBreakpoint(bp: Breakpoint): Unit = ???
}