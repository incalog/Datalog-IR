package inca.frontend.oodl.compile

import inca.frontend.oodl.syntax.Module
import inca.frontend.oodl.typechecker.Typechecker
import inca.ir.util.SourceLocation
import inca.ir.{BaseIR, CompiledModule, Name, Module as IRModule}
import inca.ir.extension.{aggregateset, block, bool, datamatch, demand, disjunction, impure, mono, not, set, tuple}
import inca.ir.visitors.BaseIRVisitor
import inca.util.compileroptions.CompilerOptions

case class CompiledOODLModule(fun: Module, override val compilerOptions: CompilerOptions) extends CompiledModule:

  override def name: Name = fun.name

  override def sourceLocation: SourceLocation = fun.name

  val oodlLogging = compilerOptions("oodl_logging")
  val logTyped: Boolean = oodlLogging.readBoolean("typed")

  lazy val typed: Module = {
    val logMod = oodlLogging.readBoolean("module")
    if (logMod && !logTyped)
      printStep("Module", fun)

    val typer: Typechecker = new Typechecker
    typer.typecheck(fun)

    if (logMod && logTyped)
      printStep("Module", fun)

    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    fun
  }

  lazy val ssa: Module = {
    val compiler = new SSA
    val module = compiler.compileModule(typed)

    val logSSA = oodlLogging.readBoolean("ssa")
    if (logSSA && !logTyped)
      printStep("SSA", fun)

    val typer: Typechecker = new Typechecker
    typer.typecheck(module)

    if (logSSA && logTyped)
      printStep("SSA", fun)

    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    module
  }

  lazy val ir: IRModule = {
    val compiler = new GenerateIR
    val module = compiler.compileModule(ssa)
    module
  }

object CompiledOODLModule:
  // Important:
  // 1. Not before block
  val pipeline: List[() => BaseIRVisitor] = List(
    () => new mono.Lowering {},
    () => new aggregateset.Lowering {},
    () => new set.Lowering {},
    () => new bool.Lowering {},
    () => new datamatch.Lowering {},
    () => new not.Lowering {},
    () => new block.Lowering {},
    () => new disjunction.Lowering {},
    () => new impure.Lowering {},
    () => new demand.Lowering {},
    () => new tuple.Lowering {},
  ) // arith + string + data