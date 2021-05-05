package inca.compiler

import inca.backend.ir.GP
import inca.compiler.functional.CompiledFunctionalModule
import inca.frontend.functional.core

object Compiler {

  def compileFun(module: String,
                 compilerOptions: Options): CompiledFunctionalModule = {
    val parsed = compilerOptions.frontend.parseModule(module).get.value
//    println(parsed)
    functional.CompiledFunctionalModule(parsed, compilerOptions)
  }

  def compileFun(module: inca.frontend.functional.core.Module,
                 compilerOptions: Options): CompiledFunctionalModule = {
    functional.CompiledFunctionalModule(module, compilerOptions)
  }

  def compileGP(module: GP.Module,
                compilerOptions: Options): CompiledGPModule = {
    CompiledGPModule(module, compilerOptions)
  }

}
