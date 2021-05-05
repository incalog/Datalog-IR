package inca.compiler

import inca.backend.ir.GP
import inca.frontend.functional
import inca.frontend.constraint
import inca.compiler.constraint.CompiledConstraintModule
import inca.runtime.context.DataModel
import inca.compiler.functional.CompiledFunctionalModule
import inca.frontend.constraint.core

object Compiler {

  def compileFunctional(module: String,
                 compilerOptions: FunctionalOptions): CompiledFunctionalModule = {
    val parsed = functional.parser.Parser.parse(module)
    CompiledFunctionalModule(parsed, compilerOptions)
  }

  def compileFunctional(module: functional.core.Module,
                 compilerOptions: FunctionalOptions): CompiledFunctionalModule = {
    CompiledFunctionalModule(module, compilerOptions)
  }

  def compileConstraint(module: String,
                        compilerOptions: ConstraintOptions): CompiledConstraintModule = {
    val parsed = constraint.parser.Parser.parse(module)
    CompiledConstraintModule(parsed, compilerOptions)
  }

  def compileConstraint(module: core.Module,
                        compilerOptions: ConstraintOptions): CompiledConstraintModule = {
    CompiledConstraintModule(module, compilerOptions)
  }

  def compileGP(module: GP.Module,
                dataModel: DataModel,
                compilerOptions: Options): CompiledGPModule = {
    CompiledGPModule(module, dataModel, compilerOptions)
  }

}
