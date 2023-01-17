package inca.debugger

trait DebuggerAPI {
  def isFinished: Boolean
  def isAtBreakpoint: Boolean

  def stepped(): Unit

  def stepInto(shortCircuit: Boolean = false): Unit = {
    if (!doStepInto(shortCircuit))
      throw new IllegalStateException()
    stepped()
  }
  def stepOver(shortCircuit: Boolean = false): Unit = {
    doStepOver(shortCircuit) || resume(shortCircuit)
    stepped()
  }
  def stepOut(shortCircuit: Boolean = false): Unit = {
    doStepOut(shortCircuit) || resume(shortCircuit)
    stepped()
  }

  final def resume(shortCircuit: Boolean = false): Boolean = {
    var b = doStepOut(shortCircuit) || doStepOver(shortCircuit) || doStepInto(shortCircuit)
    while (b && !isFinished && !isAtBreakpoint)
      b = doStepOut(shortCircuit) || doStepOver(shortCircuit) || doStepInto(shortCircuit)
    b
  }

  final def resumeWithStepInto(shortCircuit: Boolean): Boolean = {
    var b = doStepInto(shortCircuit)
    while (b && !isFinished && !isAtBreakpoint)
      b = doStepInto(shortCircuit)
    b
  }

  type Breakpoint
  def addBreakpoint(bp: Breakpoint): Unit
  def removeBreakpoint(bp: Breakpoint): Unit
  def clearBreakpoints(): Unit

  /** internal step into */
  protected def doStepInto(shortCircuit: Boolean): Boolean

  /** internal step over */
  protected def doStepOver(shortCircuit: Boolean): Boolean

  /** internal step out */
  protected def doStepOut(shortCircuit: Boolean): Boolean

}
