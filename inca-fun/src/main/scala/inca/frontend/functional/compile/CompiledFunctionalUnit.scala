package inca.frontend.functional.compile

import inca.frontend.functional.foreign
import inca.frontend.functional.syntax.Module
import inca.frontend.functional.typechecker.Typechecker
import inca.ir.analysis.{IRAbstractInterpreter}
import inca.ir.extension.*
import inca.ir.extension.set.SyntacticOptimizer
import inca.ir.optimize
import inca.ir.optimize.BaseIROptimizer
import inca.ir.util.SourceLocation
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{CompiledUnit, Name, Module as IRModule}

case class CompiledFunctionalUnit(fun: Module, override val compilerOptions: FunctionalCompilerOptions)
  extends CompiledUnit:

  override def name: Name = fun.name
  override def sourceLocation: SourceLocation = fun.name

  val funLogging = compilerOptions.funLogging
  val logTyped: Boolean = funLogging.logTypeInformation

  lazy val typed: Module = {
    val logMod = funLogging.logModule
    if (logMod && !logTyped)
      printStep("Functional-Module", fun)

    val typer: Typechecker = new Typechecker
    typer.typecheck(fun)

    if (logMod && logTyped)
      printStep("Functional-Module", fun)

    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    fun
  }

  /*lazy val monoModule: Module = {
    val logMono = funLogging.readBoolean("mono")
    val mono = new Monomorph
    val module = mono.transModule(typed)

    if (logMono && !logTyped)
      printStep("Mono", module)

    val typer: Typechecker = new Typechecker
    typer.typecheck(module)

    if (logMono && logTyped)
      printStep("Mono", module)

    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    module
  }

  lazy val defunModule: Module = {
    val logDefun = funLogging.readBoolean("defun")

    val defun = new Defunctionalize
    val module = defun.transModule(monoModule)

    if (logDefun && !logTyped)
      printStep("Defun", module)

    val typer: Typechecker = new Typechecker
    typer.typecheck(module)

    if (logDefun && logTyped)
      printStep("Defun", module)

    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    module
  }*/

  lazy val normalizedFoldModule: Module = {
    val logNormalized = funLogging.logNormalizedModule

    val norm = new NormalizeFold
    val module = norm.visitModule(typed)

    if (logNormalized && !logTyped)
      printStep("Normalized", module)

    val typer: Typechecker = new Typechecker
    typer.typecheck(module)

    if (logNormalized && logTyped)
      printStep("Normalized", module)

    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    module
  }

  val isClosedWorld = true

  def otherUnits: Seq[CompiledUnit] = Seq()

  lazy val irModules: Seq[IRModule] =
    val compiler = new GenerateIR
    val module = compiler.compileModule(normalizedFoldModule)
    Seq(module)
  

object CompiledFunctionalUnit:
  val viatraPostProcessingPipeline: List[() => BaseIRVisitor] = List(
    () => new foreign.Lowering {}
  )

  //class BoolIROptimizer(analysis: IRAbstractInterpreter) extends BaseIROptimizer(analysis)

  val pipeline: List[() => BaseIRVisitor] = List(
    () => new typeparam.Lowering {},
    () => new aggregateset.Lowering {},
    () => new SyntacticOptimizer {},
    () => new set.Lowering {},
    () => new map.Lowering {},
    () => new bool.SyntacticOptimizer {},

    //() => new disjunction.Lowering {},
    //() => new optimize.AliasElimination {},
    //() => new Rewriter(aeval => new BoolIROptimizer(aeval)) {},
    () => new bool.Lowering {},
    () => new datamatch.Lowering {},
    () => new block.Lowering {},
    () => new disjunction.Lowering {},
    () => new not.Lowering {},
    () => new demand.Lowering {},
    //() => new demand.LoweringWithSupplementaries {},
    () => new tuple.Lowering {},

    () => new optimize.IdentityCastElimination {},
    () => new optimize.AliasElimination {},
    () => new optimize.RemoveDuplicatedRelations {}
  ) // arith + string + data
