package inca.frontend.objectoriented.compiler

import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.Name
import inca.backend.ir.util.printer.DatalogPrinter
import inca.compiler.{CompiledModule, CompilerFlags, SourceLocation}
import inca.frontend.objectoriented.core.Module
import inca.frontend.objectoriented.lowering.{GenerateDataModel, GenerateDatalog, StaticSingleAssignment}
import inca.frontend.objectoriented.typechecker.Typechecker
import inca.runtime.context.DataModel

case class CompiledObjectModule(fun: Module, options: ObjectOptions) extends CompiledModule {

  override def name: Name = fun.name.raw

  override def sourceLocation: SourceLocation = fun.name

  lazy val typer: Typechecker = new Typechecker {}

  lazy val typed: Module = {
    typer.typecheck(fun)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()

    if (CompilerFlags.DEBUGMODE) {
      println(s"Typed Module")
      println(fun)
    }
    fun
  }

  lazy val ssaModule: Module = {
    val module = new StaticSingleAssignment(typed).transModule()

    typer.typecheck(module)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()

    if (CompilerFlags.DEBUGMODE) {
      println(s"SSA Module")
      println(module)
    }
    module
  }

  lazy val coreModule: Module = {
    ssaModule
    /*val module = new Defunctionalize(monoModule).transModule()
    typer.typecheck(module)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    if (CompilerFlags.DEBUGMODE) {
      println(s"Core Module")
      println(module)
    }
    module*/
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
    new GenerateDataModel(coreModule).transModule()
  }
}
