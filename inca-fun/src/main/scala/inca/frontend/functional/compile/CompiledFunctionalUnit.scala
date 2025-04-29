package inca.frontend.functional.compile

import inca.frontend.functional.foreign
import inca.frontend.functional.syntax.Module
import inca.frontend.functional.typechecker.Typechecker
import inca.ir.analysis.IRTerminationAnalysis
import inca.ir.extension.*
import inca.ir.optimize.{IRDataKindOptimizer, Optimizer, ReplaceSingletonVariables}
import inca.ir.util.SourceLocation
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{CompiledUnit, Name, optimize, Module as IRModule}
import inca.util.printStep

case class CompiledFunctionalUnit(fun: Module, override val compilerOptions: FunctionalCompilerOptions)
  extends CompiledUnit:

  override def name: Name = fun.name

  override def sourceLocation: SourceLocation = fun.name

  val funLogging = compilerOptions.funLogging
  val logTyped: Boolean = funLogging.logTypeInformation

  lazy val typed: Module = {
    val logMod = funLogging.logModule
    if (logMod && !logTyped)
      printStep("Functional-Module", fun.toString)

    val typer: Typechecker = new Typechecker
    typer.typecheck(fun)

    if (logMod && logTyped)
      printStep("Functional-Module", fun.toString)

    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    fun
  }

  lazy val normalizedFoldModule: Module = {
    val logNormalized = funLogging.logNormalizedModule

    val norm = new NormalizeFold
    val module = norm.visitModule(typed)

    if (logNormalized && !logTyped)
      printStep("Normalized", module.toString)

    val typer: Typechecker = new Typechecker
    typer.typecheck(module)

    if (logNormalized && logTyped)
      printStep("Normalized", module.toString)

    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    module
  }

  val isClosedWorld = true

  val otherUnits: Seq[CompiledUnit] = Seq()

  lazy val irModules: Seq[IRModule] =
    val compiler = new GenerateIR
    val module = compiler.compileModule(normalizedFoldModule)
    Seq(module)


object CompiledFunctionalUnit:
  val viatraPostProcessingPipeline: List[() => BaseIRVisitor] = List(
    () => new foreign.ScalaLowering {}
  )

  val ddlogPostProcessingPipeline: List[() => BaseIRVisitor] = List(
    () => new foreign.DDLogLowering {}
  )

  def createPipeline(withDemandOutlining: Boolean): List[() => BaseIRVisitor] =
    List(
      () => new typeparam.Lowering {},
      () => new aggregateset.Lowering {},
      //() => new optimize.IRConstantOptimizer(computeControlEvents = false, interRelational = true) {},
      () => new IRDataKindOptimizer(computeControlEvents = false, interRelational = true) {},
      () => new set.SyntacticOptimizer {},
      () => new set.Lowering {},
      () => new map.Lowering {},
      () => new bool.optimize.DnfOptimizer {},
      () => new bool.Lowering {},
      () => new datamatch.Lowering {},
      () => new block.Lowering {},
      () => new disjunction.Lowering {},
      () => new not.Lowering {},
      if (withDemandOutlining)
        () => new demand.LoweringWithSupplementaries {}
      else
        () => new demand.Lowering {},
      () => new tuple.Lowering {},
    ) // arith + string + data

  val pipeline: List[() => BaseIRVisitor] = createPipeline(false) // arith + string + data
  
  val optimizationPipeline: List[() => Optimizer] = List(
    () => new optimize.RemoveDuplicatedRelations {},
    () => new optimize.IRConstantOptimizer(computeControlEvents = false, interRelational = false) {},
    () => new optimize.IdentityCastElimination {},
    () => new optimize.IRConstantOptimizer(computeControlEvents = false, interRelational = true) {},
    () => new optimize.IdentityCastElimination {},
    () => new optimize.ReplaceSingletonVariables {}, // helps with detecting exact duplicates
    () => new optimize.RemoveDuplicatedRelations {},
    () => new optimize.AliasElimination {},
    () => new IRTerminationAnalysis {}
    //() => new optimize.InlineSimpleRelations {}
  )
