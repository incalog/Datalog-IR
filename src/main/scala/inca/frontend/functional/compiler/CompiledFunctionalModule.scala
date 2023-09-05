package inca.frontend.functional.compiler

import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.Name
import inca.compiler.source.SourceLocation
import inca.compiler.CompiledModule
import inca.compiler.CompilerFlags
import inca.frontend.functional.core.Module
import inca.frontend.functional.lowering.Defunctionalize
import inca.frontend.functional.lowering.GenerateDataModel
import inca.frontend.functional.lowering.GenerateDatalog
import inca.frontend.functional.lowering.Monomorph
import inca.frontend.functional.typechecker.Typechecker
import inca.runtime.context.DataModel

case class CompiledFunctionalModule(fun: Module, options: FunctionalOptions)
    extends CompiledModule {

  override def name: Name = fun.name.name

  override def sourceLocation: SourceLocation = fun.name

  lazy val typer: Typechecker = new Typechecker {}

  lazy val typed: Module = {
    typer.typecheck(fun)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
//    if (CompilerFlags.DEBUGMODE) {
//      println(s"\nTypechecked")
//      println(typed)
//    }
    fun
  }

  lazy val monoModule: Module = {
    val module = new Monomorph(typed).transModule()
    typer.typecheck(module)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    if (CompilerFlags.DEBUGMODE && typed != module) {
      println(s"\nMonomorphic Module")
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
    if (CompilerFlags.DEBUGMODE && monoModule != module) {
      println(s"\nCore Module")
      println(module)
    }
    module
  }

  lazy val ir: Datalog.Module = {
    val module = new GenerateDatalog(coreModule).transModule()
    if (CompilerFlags.DEBUGMODE) {
      println(s"\nIntermediate Representation")
      println(module)
    }
    module
  }

  lazy val dataModel: DataModel = {
    val res = new GenerateDataModel(coreModule).transModule()
    res
  }
}
