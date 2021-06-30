package inca.frontend.constraint

import inca.frontend.Frontend
import inca.frontend.constraint.compiler.{CompiledConstraintModule, ConstraintOptions}

object ConstraintFrontend extends Frontend[core.Module, ConstraintOptions] {
  override def compile(source: String, opt: ConstraintOptions): CompiledConstraintModule =
    CompiledConstraintModule(source, opt)
  override def compile(module: core.Module, opt: ConstraintOptions): CompiledConstraintModule =
    CompiledConstraintModule(module, opt)
}
