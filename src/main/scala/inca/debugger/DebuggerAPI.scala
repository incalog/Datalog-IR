package inca.debugger

trait DebuggerAPI {
  def stepInto(): Unit
  def stepOver(): Unit
  def stepOut(): Unit
  def resume(): Unit
  def resumeWithStepInto(): Unit

  type Breakpoint
  def addBreakpoint(bp: Breakpoint): Unit
  def removeBreakpoint(bp: Breakpoint): Unit
  def clearBreakpoints(): Unit
}
