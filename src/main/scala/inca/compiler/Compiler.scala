package inca.compiler

import inca.backend.ir.GP
import inca.frontend.functional
import inca.frontend.constraint
import inca.compiler.constraint.CompiledConstraintModule
import inca.runtime.context.DataModel
import inca.compiler.functional.CompiledFunctionalModule

object Compiler {

  def compileFunctional(module: String,
                 compilerOptions: Options): CompiledFunctionalModule = {
    val parsed = functional.parser.Parser.parse(module)
    CompiledFunctionalModule(parsed, compilerOptions)
  }

  def compileFunctional(module: functional.core.Module,
                 compilerOptions: Options): CompiledFunctionalModule = {
    CompiledFunctionalModule(module, compilerOptions)
  }

  def compileConstraint(module: String,
                        compilerOptions: Options): CompiledConstraintModule = {
    val parsed = constraint.parser.Parser.parse(module)
    CompiledConstraintModule(parsed, compilerOptions)
  }

  def compileConstraint(module: constraint.core.tree.Module,
                        compilerOptions: Options): CompiledConstraintModule = {
    CompiledConstraintModule(module, compilerOptions)
  }

  def compileGP(module: GP.Module,
                dataModel: DataModel,
                compilerOptions: Options): CompiledGPModule = {
    CompiledGPModule(module, dataModel, compilerOptions)
  }

}
