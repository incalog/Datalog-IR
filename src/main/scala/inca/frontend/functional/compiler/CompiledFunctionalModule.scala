package inca.frontend.functional.compiler

import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.Name
import inca.compiler.{CompiledModule, CompilerFlags, SourceLocation}
import inca.frontend.functional.core.Module
import inca.frontend.functional.lowering.{Defunctionalize, GenerateDataModel, GenerateDatalog, Monomorph}
import inca.frontend.functional.typechecker.Typechecker
import inca.runtime.context.DataModel

case class CompiledFunctionalModule(fun: Module, options: FunctionalOptions) extends CompiledModule {

  override def name: Name = fun.name.name

  override def sourceLocation: SourceLocation = fun.name

  lazy val typer = new Typechecker {}

  lazy val typed: Module = {
    typer.typecheck(fun)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    fun
  }

  lazy val monoModule: Module = {
    val module = new Monomorph(typed).transModule()
    typer.typecheck(module)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    println(module)
    if (CompilerFlags.DEBUGMODE) {
      println(s"Monomorphic Module")
      println(module)
    }
    module
  }

  lazy val coreModule: Module = {
    val module = new Defunctionalize(monoModule).transModule()
    typer.typecheck(module)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    if (CompilerFlags.DEBUGMODE) {
      println(s"Core Module")
      println(module)
    }
    module
  }

  lazy val ir: Datalog.Module = {
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
