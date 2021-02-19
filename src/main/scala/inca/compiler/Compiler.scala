package inca.compiler

import inca.backend.ir.GP
import inca.frontend.core
import inca.frontend_old

object Compiler {

  def compileFun(module: String,
                 compilerOptions: Options): CompiledFunModule = {
    val parsed = compilerOptions.frontend.parseModule(module).get.value
//    println(parsed)
    CompiledFunModule(parsed, compilerOptions)
  }

  def compileFun_old(module: String,
                 compilerOptions: Options): CompiledFunModule_old = {
    val parsed = compilerOptions.frontendOld.parseModule(module).get.value
    //    println(parsed)
    CompiledFunModule_old(parsed, compilerOptions)
  }

  def compileFun(module: core.Module,
                 compilerOptions: Options): CompiledFunModule = {
    CompiledFunModule(module, compilerOptions)
  }

  def compileFun(module: frontend_old.core.tree.Module,
                 compilerOptions: Options): CompiledFunModule_old = {
    CompiledFunModule_old(module, compilerOptions)
  }


  def compileGP(module: GP.Module,
                compilerOptions: Options): CompiledGPModule = {
    CompiledGPModule(module, compilerOptions)
  }

}
