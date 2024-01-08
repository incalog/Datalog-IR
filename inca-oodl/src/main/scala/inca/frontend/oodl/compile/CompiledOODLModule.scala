package inca.frontend.oodl.compile

import inca.frontend.oodl.syntax.Module
import inca.frontend.oodl.typechecker.Typechecker
import inca.ir.util.SourceLocation
import inca.ir.{BaseIR, CompiledModule, Name, Module as IRModule}
import inca.ir.extension.{aggregateset, block, bool, datamatch, demand, disjunction, impure, mono, not, set, tuple}
import inca.ir.visitors.BaseIRVisitor
import inca.frontend.oodl.foreign
import inca.foreign.scala.ir.mono.MonoLowering as MonoScalaLowering
import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.primitive.ConversionElimination
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker}

case class CompiledOODLModule(fun: Module, override val compilerOptions: OODLCompilerOptions) extends CompiledModule:

  override def name: Name = fun.name

  override def sourceLocation: SourceLocation = fun.name

  private class OODLTypeChecker extends IRTypechecker with primitive.Typechecker
  override def typechecker: BaseIRTypechecker = new OODLTypeChecker()

  val oodlLogging = compilerOptions.oodlLogging
  val logTyped: Boolean = oodlLogging.logTypeInformation

  lazy val viatraPostProcessingPipeline: List[() => BaseIRVisitor] = List(
    () => new foreign.Lowering(typed)
  )

  lazy val typed: Module = {
    val logMod = oodlLogging.logModule
    if (logMod && !logTyped)
      printStep("OODL-Module", fun)

    val typer: Typechecker = new Typechecker
    typer.typecheck(fun)

    if (logMod && logTyped)
      printStep("OODL-Module", fun)

    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    fun
  }

  lazy val ssa: Module = {
    val compiler = new SSA
    val module = compiler.compileModule(typed)

    val logSSA = oodlLogging.logSSAModule
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
  // 2. Impure before Disjunction
  val pipeline: List[() => BaseIRVisitor] = List(
    () => new mono.Lowering {},
    () => new MonoScalaLowering {},
    () => new ConversionElimination {},
    () => new aggregateset.Lowering {},
    () => new set.Lowering {},
    () => new bool.Lowering {},
    () => new datamatch.Lowering {},
    () => new not.Lowering {},
    () => new block.Lowering {},
    () => new impure.Lowering {},
    () => new disjunction.Lowering {},
    () => new not.Lowering {},
    () => new demand.Lowering {},
    () => new tuple.Lowering {},
  ) // arith + string + data