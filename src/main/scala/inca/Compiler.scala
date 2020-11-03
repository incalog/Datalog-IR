package inca

import inca.backend.ir.GP
import inca.frontend.core.Core

object Compiler {

  case class CompilationFailed(msg: String) extends Exception

  def compileFun(module: Core.Module,
                 compilerOptions: CompilerOptions): CompiledFunModule = {
    CompiledFunModule(module, compilerOptions)
  }

  def compileGP(module: GP.Module,
                compilerOptions: CompilerOptions): CompiledGPModule = {
    CompiledGPModule(module, compilerOptions)
  }

}
