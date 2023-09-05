package inca.compiler

import inca.backend.ir.Datalog
import inca.compiler.source.SourceString
import inca.frontend.constraint
import inca.frontend.constraint.compiler.CompiledConstraintModule
import inca.frontend.constraint.compiler.ConstraintOptions
import inca.frontend.constraint.core
import inca.frontend.functional
import inca.frontend.functional.compiler.CompiledFunctionalModule
import inca.frontend.functional.compiler.FunctionalOptions
import inca.runtime.context.DataModel

object Compiler {

  def compileFunctional(
      module: String,
      compilerOptions: FunctionalOptions
    ): CompiledFunctionalModule = {
    val parsed = functional.parser.Parser.parse(SourceString(module))
    CompiledFunctionalModule(parsed, compilerOptions)
  }

  def compileFunctional(
      module: functional.core.Module,
      compilerOptions: FunctionalOptions
    ): CompiledFunctionalModule = {
    CompiledFunctionalModule(module, compilerOptions)
  }

  def compileConstraint(
      module: String,
      compilerOptions: ConstraintOptions
    ): CompiledConstraintModule = {
    val parsed = constraint.parser.Parser.parse(module)
    CompiledConstraintModule(parsed, compilerOptions)
  }

  def compileConstraint(
      module: core.Module,
      compilerOptions: ConstraintOptions
    ): CompiledConstraintModule = {
    CompiledConstraintModule(module, compilerOptions)
  }

  def compileGP(
      module: Datalog.Module,
      dataModel: DataModel,
      compilerOptions: Options
    ): CompiledDatalogModule = {
    CompiledDatalogModule(module, dataModel, compilerOptions)
  }

}
