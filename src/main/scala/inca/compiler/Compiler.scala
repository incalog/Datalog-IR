package inca.compiler

import inca.backend.ir.GP
import inca.frontend.core.Core

object Compiler {

  def compileFun(module: Core.Module,
                 compilerOptions: Options): CompiledFunModule = {
    CompiledFunModule(module, compilerOptions)
  }

  def compileGP(module: GP.Module,
                compilerOptions: Options): CompiledGPModule = {
    CompiledGPModule(module, compilerOptions)
  }

}
