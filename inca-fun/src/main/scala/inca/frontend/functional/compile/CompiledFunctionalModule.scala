package inca.frontend.functional.compile

import inca.frontend.functional.foreign
import inca.frontend.functional.syntax.Module
import inca.frontend.functional.typechecker.Typechecker
import inca.ir.extension.*
import inca.ir.util.SourceLocation
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{CompiledModule, Name, Module as IRModule}

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
    val module = norm.visitModule(typed)
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
    println(normalizedFoldModule)
    val module = compiler.compileModule(normalizedFoldModule)
    println("\n~~~~~~~~~~~~~~~~~~~~~~~\n" + module)
    module
  }

object CompiledFunctionalModule:
  val viatraPostProcessingPipeline: List[() => BaseIRVisitor] = List(
    () => new foreign.Lowering {}
  )

  val pipeline: List[() => BaseIRVisitor] = List(
    () => new aggregateset.Lowering {},
    () => new set.Lowering {},
    () => new map.Lowering {},
    () => new bool.Optimizer {},
    () => new bool.Lowering {},
    () => new datamatch.Lowering {},
    () => new block.Lowering {},
    () => new disjunction.Lowering {},
    () => new not.Lowering {},
    () => new typeparam.Lowering {},
    () => new demand.Lowering {},
    () => new tuple.Lowering {}
  ) // arith + string + data
