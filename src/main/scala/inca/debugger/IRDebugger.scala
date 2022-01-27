package inca.debugger
import inca.backend.ir.Datalog
import inca.compiler.CompiledModule
import inca.debugger.table.Table

final class IRDebugger(compiled: CompiledModule) extends Debugger {
  super.initialize(compiled)

  override def stepIntoFrontend(): Unit = stepInto()
}