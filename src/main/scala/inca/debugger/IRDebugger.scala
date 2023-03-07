package inca.debugger

import inca.compiler.CompiledDatalogModule
import inca.runtime.db.DatabaseInput
import inca.runtime.DatalogRuntime
import scala.collection.mutable.ListBuffer

trait IRDebugger extends Debugger {
  override def doStepInto(shortCircuit: Boolean): Boolean = doStepIntoIR(shortCircuit)
  override def doStepOver(shortCircuit: Boolean): Boolean = doStepOverIR(shortCircuit)
  override def doStepOut(shortCircuit: Boolean): Boolean = doStepOutIR(shortCircuit)

  override type Breakpoint = IRBreakpoint
  override def addBreakpoint(bp: Breakpoint): Unit = breakpointHandler.addBreakpoint(bp)
  override def removeBreakpoint(bp: Breakpoint): Unit = breakpointHandler.removeBreakpoint(bp)
  override def clearBreakpoints(): Unit = breakpointHandler.clearBreakpoints()

  private val _irControlTrace: ListBuffer[Query] = ListBuffer.empty
  def irControlTrace: Seq[Query] = _irControlTrace.toSeq
  def clearIRControlTrace(): Unit = {
    _irControlTrace.clear()
  }
  def stepped(): Unit = _irControlTrace += queryStack.top
}
final class InitializingIRDebugger(
    module: CompiledDatalogModule,
    input: DatabaseInput)
    extends IRDebugger {
  this.initialize(module)
  this.initializeDatabaseRuntime(input)
  override def debuggingState: DatalogRuntime => DebuggerState = (rt: DatalogRuntime) =>
    new ResettingDebuggerState(rt)

}

final class ExternallyInitializableDebugger(module: CompiledDatalogModule) extends IRDebugger {
  super.initialize(module)

  override def debuggingState: DatalogRuntime => DebuggerState = (rt: DatalogRuntime) =>
    new AccumulatingDebuggerState(rt)
  def setRuntime(runtime: DatalogRuntime): Unit = {
    state = new AccumulatingDebuggerState(runtime)
  }
}
