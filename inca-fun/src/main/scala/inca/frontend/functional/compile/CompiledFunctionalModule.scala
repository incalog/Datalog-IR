package inca.frontend.functional.compile

import inca.ir.{CompiledModule, Name, Module as IRModule}
import inca.frontend.functional.syntax.Module
import inca.frontend.functional.typechecker.Typechecker
import inca.ir.util.SourceLocation
import inca.viatra.compile.{GeneratePSystem, PSystem}
import inca.util.ScalaCompiler

case class CompiledFunctionalModule(fun: Module) extends CompiledModule {

  override def name: Name = fun.name

  override def sourceLocation: SourceLocation = fun.name

  lazy val scalaCompiler = new ScalaCompiler()

  lazy val typed: Module = {
    val typer: Typechecker = new Typechecker
    typer.typecheck(fun)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    fun
  }

  lazy val monoModule: Module = {
    val mono = new Monomorph
    val module = mono.transModule(typed)
    val typer: Typechecker = new Typechecker
    typer.typecheck(module)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    module
  }

  lazy val defunModule: Module = {
    val defun = new Defunctionalize
    val module = defun.transModule(monoModule)
    val typer: Typechecker = new Typechecker
    typer.typecheck(module)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    module
  }

  lazy val ir: IRModule = {
    val compiler = new GenerateIR
    val module = compiler.compileModule(defunModule)
    module
  }
}
