package inca.frontend.oodl.compile

import inca.frontend.oodl.syntax.{Module, TName}
import inca.frontend.oodl.typechecker.Typechecker
import inca.ir.util.SourceLocation
import inca.ir.{BaseIR, CompiledUnit, Name, Module as IRModule}
import inca.ir.extension.{aggregateset, block, bool, datamatch, demand, disjunction, impure, map, mono, not, set, tuple}
import inca.ir.visitors.BaseIRVisitor
import inca.frontend.oodl.foreign
import inca.foreign.scala.ir.mono.MonoLowering as MonoScalaLowering
import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.primitive.ConversionElimination
import inca.frontend.oodl.compile.CompiledOODLUnit.{createOptimizationPipeline, createPipeline}
import inca.ir.optimize as iroptimize
import inca.ir.optimize.{AbstractEdbConfig, IROODLClassOptimizer, IdentityCastElimination, OODLEdbConfig, Optimizer}
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker}
import inca.util.printStep

case class CompiledOODLUnit(fun: Module, override val compilerOptions: OODLCompilerOptions) extends CompiledUnit:

  override def name: Name = fun.name

  override def sourceLocation: SourceLocation = fun.name

  private class OODLTypeChecker extends IRTypechecker with primitive.Typechecker

  override def typechecker: BaseIRTypechecker = new OODLTypeChecker()

  val oodlLogging: OODLLoggingSection = compilerOptions.oodlLogging
  val logTyped: Boolean = oodlLogging.logTypeInformation

  lazy val viatraPostProcessingPipeline: List[() => BaseIRVisitor] = List(
    () => new foreign.Lowering(typed)
  )

  lazy val typed: Module =
    val logMod = oodlLogging.logModule
    if (logMod && !logTyped)
      printStep("OODL-Module", fun.toString)

    val typer: Typechecker = new Typechecker
    typer.typecheck(fun)

    if (logMod && logTyped)
      printStep("OODL-Module", fun.toString)

    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    fun

  lazy val ssa: Module =
    val compiler = new SSA
    val module = compiler.compileModule(typed)

    val logSSA = oodlLogging.logSSAModule
    if (logSSA && !logTyped)
      printStep("SSA", fun.toString)

    val typer: Typechecker = new Typechecker
    typer.typecheck(module)

    if (logSSA && logTyped)
      printStep("SSA", fun.toString)

    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    module

  val isClosedWorld = true

  def otherUnits: Seq[CompiledUnit] = Seq()

  lazy val irModules: Seq[IRModule] =
    val compiler = new GenerateIR
    val module = compiler.compileModule(ssa)
    Seq(module)

  val pipeline: List[() => BaseIRVisitor] = createPipeline(false)

  val optimizationPipeline: List[() => Optimizer] = createOptimizationPipeline(false, OODLEdbConfig.default)

  private lazy val superClassMap: Map[String, Set[String]] =
    val classes = typed.classes
    val noneTransitiveSubtypeTuples = classes.flatMap { c =>
      val directParents =
        if (c.parentCls.isEmpty)
          Seq((c.name.name, "Object"))
        else
          c.parentCls.map {
            case p: TName => (c.name.name, p.name.name)
            case t => throw IllegalStateException(s"Unexpected parent class type $t")
          }
      directParents :+ ("Null", c.name.name)
    }.distinct :+ ("Null", "Object") :+ ("Object", "Object")
    noneTransitiveSubtypeTuples
      .groupBy(_._1)
      .view.mapValues(_.map(_._2).toSet)
      .toMap
  
  def createPipeline(withDemandOutlining: Boolean): List[() => BaseIRVisitor] =
    CompiledOODLUnit.createPipeline(withDemandOutlining) 
    //++ CompiledOODLUnit.optimizationPipeline 
    //:+ (() => new IROODLClassOptimizer(superClassMap, false, true, OODLEdbConfig.default))

  def createOptimizationPipeline(computeControlEvents: Boolean, edbConfig: AbstractEdbConfig): List[() => Optimizer] =
    List(
      //() => new optimize.TypeIROptimizer {},
      () => new iroptimize.IRConstantOptimizer(computeControlEvents, false, edbConfig),
      () => new iroptimize.IdentityCastElimination {},
      () => new IROODLClassOptimizer(superClassMap, computeControlEvents, true, edbConfig),
      //() => new optimize.IdentityCastElimination {},
      //() => new optimize.AliasElimination {},
      () => new iroptimize.IRConstantOptimizer(computeControlEvents, true, edbConfig),
      //() => new iroptimize.IRConstantOptimizer(computeControlEvents, false, edbConfig),
      () => new iroptimize.IdentityCastElimination {},
      () => new iroptimize.AliasElimination {}
    )


object CompiledOODLUnit:
  // Important:
  // 1. Not before block
  // 2. Impure before Disjunction
  val pipeline: List[() => BaseIRVisitor] = createPipeline(false)

  val optimizationPipeline: List[() => Optimizer] = createOptimizationPipeline(false, OODLEdbConfig.default)
  
  def createPipeline(withDemandOutlining: Boolean): List[() => BaseIRVisitor] =
    List(
      () => new mono.Lowering(optimizeMono = true) {},
      () => new MonoScalaLowering {},
      () => new ConversionElimination {},
      () => new aggregateset.Lowering {},
      () => new set.Lowering {},
      () => new map.Lowering {},
      () => new bool.Lowering {},
      () => new datamatch.Lowering {},
      () => new not.Lowering {},
      () => new block.Lowering {},
      () => new impure.Lowering {},
      () => new disjunction.Lowering {},
      () => new not.Lowering {},
      if (withDemandOutlining)
        () => new demand.LoweringWithSupplementaries {}
      else
        () => new demand.Lowering {},
      () => new tuple.Lowering {},

      () => new iroptimize.IdentityCastElimination {},
      () => new iroptimize.AliasElimination {},
      () => new iroptimize.RemoveDuplicatedRelations {}
    ) // arith + string + data

  def createOptimizationPipeline(computeControlEvents: Boolean, edbConfig: AbstractEdbConfig) : List[() => Optimizer] = List(
    //() => new optimize.TypeIROptimizer {},
    () => new iroptimize.IRConstantOptimizer(computeControlEvents, false, edbConfig),
    () => new iroptimize.IdentityCastElimination {},
    //() => new optimize.IdentityCastElimination {},
    //() => new optimize.AliasElimination {},
    () => new iroptimize.IRConstantOptimizer(computeControlEvents, true, edbConfig),
    () => new iroptimize.IdentityCastElimination {},
    () => new iroptimize.AliasElimination {}
  )