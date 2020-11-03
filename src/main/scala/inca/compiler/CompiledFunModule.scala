package inca.compiler

import inca.backend.ir.GP
import inca.backend.ir.GP.Name
import inca.frontend.core._
import inca.frontend.desugar.Desugar
import inca.frontend.parser.SourceLocation

case class CompiledFunModule(fun: Module, compilerOptions: Options) extends CompiledModule {

  override def name: Name = fun.name.name

  override def sourceLocation: SourceLocation = fun.name

  lazy val typed: Core.Module = {
    val frontend = compilerOptions.frontend
    frontend.typecheck(fun)
    messages ++= frontend.getErrors
    messages ++= frontend.getWarnings
    fun
  }

  lazy val desugared: Core.Module = {
    val frontend = compilerOptions.frontend
    val module = Desugar(frontend.allDesugarables)(typed)
    frontend.typecheck(module)
    messages ++= frontend.getErrors
    messages ++= frontend.getWarnings
    module
  }

  lazy val ir: GP.Module =
    CompileToGP.transformModule(desugared)
}
