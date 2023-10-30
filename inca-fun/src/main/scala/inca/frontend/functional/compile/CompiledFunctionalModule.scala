package inca.frontend.functional.compile

import inca.frontend.functional.foreign.Lowering
import inca.ir.{BaseIR, CompiledModule, Name, Module as IRModule}
import inca.frontend.functional.syntax.Module
import inca.frontend.functional.typechecker.Typechecker
import inca.ir.util.SourceLocation
import inca.ir.visitors.BaseIRVisitor

case class CompiledFunctionalModule(fun: Module) extends CompiledModule:

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

  lazy val normalizedFoldModule: Module = {
    val norm = new NormalizeFold
    val module = norm.visitModule(defunModule)
    println(module)
    val typer: Typechecker = new Typechecker
    typer.typecheck(module)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    module
  }

  lazy val ir: IRModule = {
    val compiler = new GenerateIR
    val module = compiler.compileModule(normalizedFoldModule)
    module
  }

  // lower all our functional code for folding without typechecking
  override lazy val lowered: IRModule =
    val scalaLowering = () => new Lowering {}
    scalaLowering().lower(super.lowered)
