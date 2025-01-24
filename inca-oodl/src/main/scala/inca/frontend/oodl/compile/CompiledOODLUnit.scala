package inca.frontend.oodl.compile

import inca.frontend.oodl.syntax.Module
import inca.frontend.oodl.typechecker.Typechecker
import inca.ir.util.SourceLocation
import inca.ir.{BaseIR, CompiledUnit, Name, Module as IRModule}
import inca.ir.extension.{aggregateset, block, bool, datamatch, demand, disjunction, impure, map, mono, not, set, tuple}
import inca.ir.visitors.BaseIRVisitor
import inca.frontend.oodl.foreign
import inca.foreign.scala.ir.mono.MonoLowering as MonoScalaLowering
import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.primitive.ConversionElimination
import inca.ir.optimize
import inca.ir.optimize.Optimizer
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker}
import inca.util.printStep

case class CompiledOODLUnit(fun: Module, override val compilerOptions: OODLCompilerOptions) extends CompiledUnit:

  override def name: Name = fun.name

  override def sourceLocation: SourceLocation = fun.name

  private class OODLTypeChecker extends IRTypechecker with primitive.Typechecker

  override def typechecker: BaseIRTypechecker = new OODLTypeChecker()

  val oodlLogging = compilerOptions.oodlLogging
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

/*override lazy val lowered: IRModule =
  val low = super.lowered
  println(low)
  low*/

object CompiledOODLUnit:
  // Important:
  // 1. Not before block
  // 2. Impure before Disjunction
  val pipeline: List[() => BaseIRVisitor] = createPipeline(false)

  def createPipeline(withDemandOutlining: Boolean): List[() => BaseIRVisitor] =
    val demandLowering = () => {
      if withDemandOutlining then
        new demand.LoweringWithSupplementaries {}
      else
        new demand.Lowering {}
    }
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
      demandLowering,
      () => new tuple.Lowering {},

      () => new optimize.IdentityCastElimination {},
      () => new optimize.AliasElimination {},
      () => new optimize.RemoveDuplicatedRelations {}
    ) // arith + string + data

  val optimizationPipeline: List[() => Optimizer] = List(
    //() => new optimize.TypeIROptimizer {},
    () => new optimize.IRConstantOptimizer(assumeEdbIsNotEmpty = true, computeControlEvents = false, interRelational = true) {},
    () => new optimize.IdentityCastElimination {},
    () => new optimize.AliasElimination {}
  )