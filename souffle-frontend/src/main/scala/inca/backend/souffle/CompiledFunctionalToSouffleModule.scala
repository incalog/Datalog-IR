package inca.backend.souffle

import inca.frontend.functional.compiler.{CompiledFunctionalModule, FunctionalOptions}
import inca.frontend.functional.core.{DataDef, Module}

object CompiledFunctionalToSouffleModule {
  def apply(src: String, options: FunctionalOptions = FunctionalOptions()): CompiledFunctionalToSouffleModule = {
    val fun = inca.frontend.functional.parser.Parser.parse(src)
    new CompiledFunctionalToSouffleModule(fun, options)
  }
}
class CompiledFunctionalToSouffleModule(fun: Module, options: FunctionalOptions) extends CompiledFunctionalModule(fun, options) {
  // TODO check if module contains fold and abort
  lazy val souffleSource: String = {
    val compiler = new GenerateSouffle
    compiler.compileModule(optimized, fun.content.collect{case d: DataDef => d}, dataModel)
  }
}
