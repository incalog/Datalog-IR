package inca.compiler

import fastparse.Parsed
import inca.backend.ir.GP
import inca.compiler.constraint.CompiledConstraintModule
import inca.frontend.constraint.core
import inca.frontend.constraint.extensions
import inca.frontend.constraint.parser.CoreParser
import inca.runtime.context.DataModel

object Compiler {

  private lazy val parser: CoreParser = new CoreParser
    with extensions.boolOps.Parser
    with extensions.evalCall.Parser
    with extensions.forallExists.Parser
    with extensions.foreach.Parser
    with extensions.ifThenElse.Parser
    with extensions.match_.Parser
    with extensions.switch_.Parser {}

  def compileFun(code: String, compilerOptions: Options): CompiledConstraintModule = {
    val module =
      fastparse.parse(code, parser.module(_), verboseFailures = true) match {
        case Parsed.Success(value, _) => value
        case fail: Parsed.Failure =>
          throw new IllegalArgumentException(s"Parsing Error: ${fail.trace(true).longTerminalsMsg}")
      }
    constraint.CompiledConstraintModule(module, compilerOptions)
  }

  def compileFun(module: core.tree.Module,
                 compilerOptions: Options): CompiledConstraintModule = {
    constraint.CompiledConstraintModule(module, compilerOptions)
  }

  def compileGP(module: GP.Module,
                langMetaInfo: DataModel,
                compilerOptions: Options): CompiledGPModule = {
    CompiledGPModule(module, langMetaInfo, compilerOptions)
  }

}
