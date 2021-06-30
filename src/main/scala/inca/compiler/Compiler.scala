package inca.compiler

import inca.backend.ir
import inca.backend.ir.{CompiledIRModule, IR}
import inca.frontend.constraint.compiler.{CompiledConstraintModule, ConstraintOptions}
import inca.frontend.constraint.core
import inca.frontend.datalog.compiler.{CompiledDatalogModule, DatalogOptions}
import inca.frontend.functional.compiler.{CompiledFunctionalModule, FunctionalOptions}
import inca.frontend.{Frontend, constraint, datalog, functional}
import inca.runtime.context.DataModel

import scala.io.Source

object Compiler {

  def compileFile[M, Opt <: Options](path: String, options: Opt, front: Frontend[M, Opt]): CompiledModule = {
    val reader = Source.fromFile(path)
    val source = reader.getLines().mkString
    reader.close()
    front.compile(source, options)
  }

  def compileSource[M, Opt <: Options](source: String, options: Opt, front: Frontend[M, Opt]): CompiledModule =
    front.compile(source, options)

  def compileParsed[M, Opt <: Options](module: M, options: Opt, front: Frontend[M, Opt]): CompiledModule =
    front.compile(module, options)

  def compileDatalog(module: String,
                     compilerOptions: DatalogOptions): CompiledDatalogModule = {
    val parsed = datalog.syntax.Parser.parse(module)
    CompiledDatalogModule(parsed, compilerOptions)
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

  def compileGP(module: IR.Module,
                dataModel: DataModel,
                compilerOptions: Options): CompiledIRModule = {
    ir.CompiledIRModule(module, dataModel, compilerOptions)
  }

}
