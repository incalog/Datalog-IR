package inca.frontend

import inca.compiler.{CompiledModule, Options}

trait Frontend[Mod, Opt <: Options] {
  def compile(source: String, opt: Opt): CompiledModule
  def compile(module: Mod, opt: Opt): CompiledModule
}
