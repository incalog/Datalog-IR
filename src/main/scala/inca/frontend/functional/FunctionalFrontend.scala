package inca.frontend.functional

import inca.frontend.Frontend
import inca.frontend.functional.compiler.{CompiledFunctionalModule, FunctionalOptions}

object FunctionalFrontend extends Frontend[core.Module, FunctionalOptions] {
  override def compile(source: String, opt: FunctionalOptions): CompiledFunctionalModule =
    CompiledFunctionalModule(source, opt)
  override def compile(module: core.Module, opt: FunctionalOptions): CompiledFunctionalModule =
    CompiledFunctionalModule(module, opt)
}
