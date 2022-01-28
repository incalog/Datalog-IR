package inca.debugger
import inca.compiler.CompiledModule

final class IRDebugger(compiled: CompiledModule) extends Debugger {
  super.initialize(compiled)
}