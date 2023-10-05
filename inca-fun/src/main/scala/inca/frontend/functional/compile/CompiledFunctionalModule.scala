package inca.frontend.functional.compile

import inca.ir.{CompiledModule, Name, Module => IRModule}
import inca.frontend.functional.syntax.Module
import inca.frontend.functional.typechecker.Typechecker
import inca.ir.util.SourceLocation

case class CompiledFunctionalModule(fun: Module) extends CompiledModule {

  override def name: Name = fun.name

  override def sourceLocation: SourceLocation = fun.name

  lazy val typed: Module = {
    val typer: Typechecker = new Typechecker
    typer.typecheck(fun)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    fun
  }

  lazy val monoModule: Module = {
    val module = typed // new Monomorph(typed).transModule()
    val typer: Typechecker = new Typechecker
    typer.typecheck(module)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    module
  }

  lazy val defunModule: Module = {
    val module = monoModule // new Defunctionalize(monoModule).transModule()
    val typer: Typechecker = new Typechecker
    typer.typecheck(module)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    module
  }

  lazy val ir: IRModule = {
    val compiler = new GenerateDatalog
    val module = compiler.compileModule(defunModule)
    module
  }
}
