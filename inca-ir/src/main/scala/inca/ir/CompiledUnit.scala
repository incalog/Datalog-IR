package inca.ir

import inca.ir.optimize.{BaseIROptimizer, Optimizer}
import inca.ir.printer.GenericPrinter
import inca.ir.typing.{BaseIRTypechecker, DependencyGraph, IRTypechecker}
import inca.ir.util.SourceLocation
import inca.ir.visitors.{BaseIRVisitor, IRVisitor, StatisticsCollector}
import inca.util.{CompilationMessage, printStep, printSteps}
import inca.util.compileroptions.CompilerOptions
import inca.util.DEFAULT_PRINTER

import scala.collection.mutable.ListBuffer

trait CompiledUnit(using implicit val printer: GenericPrinter = DEFAULT_PRINTER):
  def compilerOptions: CompilerOptions
  def name: Name
  def sourceLocation: SourceLocation
  def isClosedWorld: Boolean
  val irModules: Seq[Module]
  val otherUnits: Seq[CompiledUnit]

  def compiled: Seq[Module] = closed
  
  lazy val header: Seq[Module] = dependencies.map(_.header)
  private lazy val dependencies: Seq[Module] = otherUnits.flatMap(_.irModules)
  protected val messages: ListBuffer[CompilationMessage] = ListBuffer()

  def allMessages: List[CompilationMessage] = messages.toList
  def errors: List[CompilationMessage] = messages.filter(_.severity == CompilationMessage.ERROR).toList
  def warnings: List[CompilationMessage] = messages.filter(_.severity == CompilationMessage.WARNING).toList

  protected def stopIfNeeded(): Unit = {
    val es = errors
    val ws = es ++ warnings
    if ( /*options.stopOnWarning &&*/ warnings.nonEmpty)
      throw CompiledUnit.Failed(this, ws)
    if ( /*options.stopOnError &&*/ errors.nonEmpty)
      throw CompiledUnit.Failed(this, es)
  }

  protected def typechecker: BaseIRTypechecker = new IRTypechecker

  protected def printStatistics(modules: Seq[Module], str: String): Unit =
    StatisticsCollector.printStatistics(modules, str)

  lazy val (checked, dependencyGraph): (Seq[Module], DependencyGraph) =
    val checker = typechecker
    checker.checkProgram(irModules, header)
    (irModules, checker.getDependencyGraph)

  def setPipeline(pipeline: List[() => BaseIRVisitor]): Unit =
    this.pipeline = pipeline

  private var pipeline: List[() => BaseIRVisitor] = List()

  def setOptimizationPipeline(pipeline: List[() => Optimizer]): Unit =
    this.optimizationPipeline = pipeline

  private var optimizationPipeline: List[() => Optimizer] = List()

  def setPostProcessingPipeline(pipeline: List[() => BaseIRVisitor]): Unit =
    this.postProcessingPipeline = pipeline

  private var postProcessingPipeline: List[() => BaseIRVisitor] = List()

  private lazy val loweredOtherUnits: Seq[Module] =
    val low = otherUnits.flatMap(_.compiled)
    /*val checker = typechecker
    checker.checkProgram(low)*/
    low

  lazy val lowered: Seq[Module] =
    val irLogging = compilerOptions.irLogging
    val logTyped = irLogging.logTypeInformation
    val logModule = irLogging.logModule
    val logLowerings = irLogging.logLowerings
    val logStatsBeforeLowering = irLogging.logStatsBeforeLowering


    //println(s"Lower $name :: ${header.size}")
    //header.foreach(println)
    //println()

    if (logModule)
      printSteps("IR-Module", if logTyped then checked else irModules)

    if (logStatsBeforeLowering)
      printStatistics(checked, "before lowering")

    stopIfNeeded()

    val loweredMods = pipeline.foldLeft(checked) { case (ms, lowering) =>
      val lowFun = lowering()
      lowFun.isClosedWorld = isClosedWorld

      val ls = lowFun.visitProgram(ms, loweredOtherUnits)

      if (logLowerings && !logTyped)
        printSteps(s"Lowering: ${lowFun.name}", ls)

      val checker = typechecker
      try checker.checkProgram(ls, header)
      finally if (logLowerings && logTyped)
        printSteps(s"Lowering: ${lowFun.name}", ls)
      ls
    }
    loweredMods

  lazy val optimized: Seq[Module] =
    val irLogging = compilerOptions.irLogging
    val logStatsBeforeOptimization = irLogging.logStatsBeforeOptimizations
    val logStatsAfterOptimization = irLogging.logStatsAfterOptimizations

    if (logStatsBeforeOptimization)
      printStatistics(lowered, s"before optimization")
    val mods = optimize(lowered)
    if (logStatsAfterOptimization)
      printStatistics(mods, s"after optimization")
    mods

  def optimize(p: Seq[Module]): Seq[Module] =
    val irLogging = compilerOptions.irLogging
    val logOptimizations = irLogging.logOptimizations
    val logTyped = irLogging.logTypeInformation
    val logAnalsis = irLogging.logAnalysis
    val logControlGraph = irLogging.logControlGraph
    val logOptimizerStats = irLogging.logOptimizationStats

    if (logOptimizations)
      printSteps(s"Before optimization", p)

    optimizationPipeline.foldLeft(p) { case (ms, optimizer) =>
      val optimFun = optimizer()

      // log analysis phase
      val anStart = System.currentTimeMillis()
      optimFun.analyzeProgram(ms)
      val anTime = System.currentTimeMillis() - anStart
      if (logAnalsis)
        printSteps(s"Analysis: ${optimFun.name}, ${anTime}ms", ms)

      // log control events
      optimFun match
        case optimizer: BaseIROptimizer[?, ?, ?] if logControlGraph && optimizer.computeControlEvents =>
          printStep(s"Control-Graph: ${optimFun.name}", optimizer.controlGraph.get)
        case _ => // nothing

      val optStart = System.currentTimeMillis()
      val ls = optimFun.visitProgram(ms, loweredOtherUnits)
      val optTime = System.currentTimeMillis() - optStart

      if (logOptimizerStats)
        println(s"Optimization: ${optimFun.name}, ${anTime + optTime}ms\n  " + optimFun.statsString)

      if (logOptimizations && !logTyped)
        printSteps(s"Optimization: ${optimFun.name}", ls)

      val checker = typechecker
      try checker.checkProgram(ls, header)
      finally if (logOptimizations && logTyped)
        printSteps(s"Optimization: ${optimFun.name}", ls)
      ls
    }

  lazy val closed: Seq[Module] =
    val logLowerings = compilerOptions.irLogging.logLowerings
    if (isClosedWorld) {
      postProcessingPipeline.foldLeft(optimized) { case (ms, lowering) =>
        val lowFun = lowering()
        val ls = lowFun.visitProgram(ms)
        // Don't typecheck after postprocessing
        if (logLowerings)
          printSteps(s"Post processing optimization: ${lowFun.name}", ls)
        ls
      }
    } else {
      optimized
    }


object CompiledUnit:
  case class Failed(module: CompiledUnit, messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))
