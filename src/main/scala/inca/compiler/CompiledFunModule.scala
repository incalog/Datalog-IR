package inca.compiler

import inca.backend.ir.GP
import inca.backend.ir.GP.Name
import inca.frontend.core.Module
import inca.frontend.lowering.{GenerateDatalog, GenerateLMI}
import inca.runtime.context.LanguageMetaInfo

case class CompiledFunModule(fun: Module, options: Options) extends CompiledModule {

  override def name: Name = fun.name.name

  override def sourceLocation: SourceLocation = fun.name

  lazy val typed: Module = {
    val frontend = options.frontend
    frontend.typecheck(fun)
    messages ++= frontend.getErrors
    messages ++= frontend.getWarnings
    stopIfNeeded()
    fun
  }

  lazy val desugared: Module = typed
//  {
//    val frontend = options.frontend
//    val module = Desugar(frontend.allDesugarables)(typed)
//    frontend.typecheck(module)
//    messages ++= frontend.getErrors
//    messages ++= frontend.getWarnings
////    println(module)
//    stopIfNeeded()
//    module
//  }

  lazy val ir: GP.Module = {
    val module = new GenerateDatalog(desugared).transModule()
//    println(module)
    module
  }

  lazy val lmi: LanguageMetaInfo = {
    val res = new GenerateLMI(desugared).transModule()
    res
  }
}
