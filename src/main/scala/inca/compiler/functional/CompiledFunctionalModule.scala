package inca.compiler.functional

import inca.backend.ir.GP
import inca.backend.ir.GP.Name
import inca.compiler.{CompiledModule, CompilerFlags, Options, SourceLocation}
import inca.frontend.functional.core.Module
import inca.frontend.functional.lowering.{Defunctionalize, GenerateDatalog, GenerateDataModel}
import inca.runtime.context.DataModel

case class CompiledFunctionalModule(fun: Module, options: Options) extends CompiledModule {

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

  lazy val coreModule: Module = {
    val module = new Defunctionalize(typed).transModule()
    val frontend = options.frontend
    frontend.typecheck(module)
    messages ++= frontend.getErrors
    messages ++= frontend.getWarnings
    stopIfNeeded()
    if (CompilerFlags.DEBUGMODE) {
      println(s"Core Module")
      println(module)
    }
    module
  }

  lazy val ir: GP.Module = {
    val module = new GenerateDatalog(coreModule).transModule()
    if (CompilerFlags.DEBUGMODE) {
      println(s"Intermediate Representation")
      println(module)
    }
    module
  }

  lazy val dataModel: DataModel = {
    val res = new GenerateDataModel(coreModule).transModule()
    res
  }
}
