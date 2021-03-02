package inca.compiler

import inca.backend.ir.GP
import inca.backend.ir.GP.Name
import inca.frontend_old.core.CompileToGP
import inca.frontend_old.core.tree._
import inca.frontend_old.desugar.Desugar
import inca.runtime.context.LanguageMetaInfo

case class CompiledFunModule_old(fun: Module, options: Options) extends CompiledModule {

  override def name: Name = fun.name.name

  override def sourceLocation: SourceLocation = fun.name

  override def lmi: LanguageMetaInfo = options.languageMetaInfo

  lazy val typed: Module = {
    val frontend = options.frontendOld
    frontend.typecheck(fun)
    messages ++= frontend.getErrors
    messages ++= frontend.getWarnings
    stopIfNeeded()
    fun
  }

  lazy val desugared: Module = {
    val frontend = options.frontendOld
    val module = Desugar(frontend.allDesugarables)(typed)
    frontend.typecheck(module)
    messages ++= frontend.getErrors
    messages ++= frontend.getWarnings
    //    println(module)
    stopIfNeeded()
    module
  }

  lazy val ir: GP.Module = {
    val module = new CompileToGP().transformModule(desugared)
    //    println(module)
    module
  }
}
