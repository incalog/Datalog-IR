package inca.debugger

trait DebuggerAPI {
  def stepInto(): Unit =
    if (!doStepInto())
      throw new IllegalStateException()
  def stepOver(): Unit = doStepOver() || resume()
  def stepOut(): Boolean = doStepOut() || resume()

  /** internal step into */
  protected def doStepInto(): Boolean
  /** internal step over */
  protected def doStepOver(): Boolean
  /** internal step out */
  protected def doStepOut(): Boolean

  final def resume(): Boolean = {
    var b = doStepOut() || doStepOver() || doStepInto()
    while (b && !isFinished && !isAtBreakpoint)
      b = doStepOut() || doStepOver() || doStepInto()
    b
  }

  final def resumeWithStepInto(): Boolean = {
    var b = doStepInto()
    while (b && !isFinished && !isAtBreakpoint)
      b = doStepInto()
    b
  }

  def isFinished: Boolean
  def isAtBreakpoint: Boolean

  type Breakpoint
  def addBreakpoint(bp: Breakpoint): Unit
  def removeBreakpoint(bp: Breakpoint): Unit
  def clearBreakpoints(): Unit
}
