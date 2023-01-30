package inca.debugger

import inca.compiler.CompiledDatalogModule
import inca.runtime.db.DatabaseInput
import scala.collection.mutable.ListBuffer

final class IRDebugger(module: CompiledDatalogModule, input: DatabaseInput) extends Debugger {

  this.initialize(module)
  this.initializeDatabaseRuntime(input)

  override def doStepInto(shortCircuit: Boolean): Boolean = doStepIntoIR(shortCircuit)
  override def doStepOver(shortCircuit: Boolean): Boolean = doStepOverIR(shortCircuit)
  override def doStepOut(shortCircuit: Boolean): Boolean = doStepOutIR(shortCircuit)

  override type Breakpoint = IRBreakpoint
  override def addBreakpoint(bp: Breakpoint): Unit = breakpointHandler.addBreakpoint(bp)
  override def removeBreakpoint(bp: Breakpoint): Unit = breakpointHandler.removeBreakpoint(bp)
  override def clearBreakpoints(): Unit = breakpointHandler.clearBreakpoints()

  private val _irControlTrace: ListBuffer[Query] = ListBuffer.empty
  def irControlTrace: Seq[Query] = _irControlTrace.toSeq
  def stepped(): Unit = _irControlTrace += queryStack.top

}
