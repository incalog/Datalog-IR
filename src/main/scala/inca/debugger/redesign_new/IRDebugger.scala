package inca.debugger.redesign_new

import inca.compiler.CompiledDatalogModule
import inca.runtime.db.DatabaseInput
import scala.collection.mutable.ListBuffer

final class IRDebugger(module: CompiledDatalogModule, input: DatabaseInput) extends Debugger {

  this.initialize(module)
  this.initializeDatabaseRuntime(input)

  override def doStepInto(): Boolean = doStepIntoIR(false)
  override def doStepOver(): Boolean = doStepOverIR(false)
  override def doStepOut(): Boolean = doStepOutIR()

  override type Breakpoint = IRBreakpoint
  override def addBreakpoint(bp: Breakpoint): Unit = breakpointHandler.addBreakpoint(bp)
  override def removeBreakpoint(bp: Breakpoint): Unit = breakpointHandler.removeBreakpoint(bp)
  override def clearBreakpoints(): Unit = breakpointHandler.clearBreakpoints()

  private val _irControlTrace: ListBuffer[Query] = ListBuffer.empty
  def irControlTrace: Seq[Query] = _irControlTrace.toSeq
  def stepped(): Unit = _irControlTrace += queryStack.top
}
