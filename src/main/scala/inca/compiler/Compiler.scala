package inca.compiler

import inca.backend.ir.Datalog
import inca.frontend.constraint.compiler.{CompiledConstraintModule, ConstraintOptions}
import inca.frontend.constraint.core
import inca.frontend.functional.compiler.{CompiledFunctionalModule, FunctionalOptions}
import inca.frontend.objectoriented.compiler.{CompiledObjectOrientedModule, ObjectOrientedOptions}
import inca.frontend.{constraint, functional, objectoriented}
import inca.runtime.context.DataModel

object Compiler {

  def compileObjectOriented(module: String,
                            compilerOptions: ObjectOrientedOptions): CompiledObjectOrientedModule = {
    val parsed = objectoriented.parser.Parser.parse(module)
    CompiledObjectOrientedModule(parsed, compilerOptions)
  }

  def compileObjectOriented(module: objectoriented.core.Module,
                        compilerOptions: ObjectOrientedOptions): CompiledObjectOrientedModule = {
    CompiledObjectOrientedModule(module, compilerOptions)
  }

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

  def compileGP(module: Datalog.Module,
                dataModel: DataModel,
                compilerOptions: Options): CompiledDatalogModule = {
    CompiledDatalogModule(module, dataModel, compilerOptions)
  }

}
