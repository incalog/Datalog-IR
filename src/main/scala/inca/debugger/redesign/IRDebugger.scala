package inca.debugger.redesign

import inca.backend.ir.Datalog
import inca.compiler.CompiledDatalogModule
import inca.debugger.Value
import inca.debugger.table.ImmutableTable
import inca.runtime.db.DatabaseInput

import scala.collection.mutable.ListBuffer

final class IRDebugger(module: CompiledDatalogModule, input: DatabaseInput) extends Debugger {

  this.initialize(module)
  this.initializeDatabaseRuntime(input)

  override def doStepInto(): Boolean = stepIntoIR()
  override def doStepOver(): Boolean = stepOverIR()
  override def doStepOut(): Boolean = stepOutIR()

  override type Breakpoint = IRBreakpoint
  override def addBreakpoint(bp: Breakpoint): Unit = breakpointHandler.addBreakpoint(bp)
  override def removeBreakpoint(bp: Breakpoint): Unit = breakpointHandler.removeBreakpoint(bp)

  private val _irControlTrace: ListBuffer[EvaluationPoint] = ListBuffer.empty
  def irControlTrace: Seq[EvaluationPoint] = _irControlTrace.toSeq
  def stepped(): Unit = _irControlTrace += callStack.top
}
