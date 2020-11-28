package inca.compiler

import inca.backend.ir.GP
import inca.frontend.core

object Compiler {

  def compileFun(module: String,
                 compilerOptions: Options): CompiledFunModule = {
    val parsed = compilerOptions.frontend.parseModule(module).get.value
//    println(parsed)
    CompiledFunModule(parsed, compilerOptions)
  }

  def compileFun(module: core.tree.Module,
                 compilerOptions: Options): CompiledFunModule = {
    CompiledFunModule(module, compilerOptions)
  }

  def compileGP(module: GP.Module,
                compilerOptions: Options): CompiledGPModule = {
    CompiledGPModule(module, compilerOptions)
  }

}
