package inca.ir

import inca.ir.extension.*
import inca.ir.analysis.IRAbstractInterpreter
import inca.ir.lowering.BaseLowering
import inca.ir.optimize.{BaseIROptimizer, IROptimizer}
import inca.ir.typing.{BaseIRTypechecker, DependencyGraph, IRTypechecker}
import inca.ir.util.SourceLocation
import inca.ir.visitors.{BaseIRVisitor, IRVisitor, StatisticsCollector}
import inca.util.CompilationMessage
import inca.util.compileroptions.CompilerOptions

import scala.collection.mutable.ListBuffer

trait CompiledUnit:
  def compilerOptions: CompilerOptions
  def name: Name
  def sourceLocation: SourceLocation

  def isClosedWorld: Boolean
  def irModules: Seq[Module]
  def otherUnits: Seq[CompiledUnit]

  lazy val header: Seq[Module] = dependencies.map(_.header)

  private lazy val dependencies: Seq[Module] = otherUnits.flatMap(_.irModules)

  protected val messages: ListBuffer[CompilationMessage] = ListBuffer()
  def allMessages: List[CompilationMessage] = messages.toList
  def errors: List[CompilationMessage] = messages.filter(_.severity == CompilationMessage.ERROR).toList
  def warnings: List[CompilationMessage] = messages.filter(_.severity == CompilationMessage.WARNING).toList

  protected def stopIfNeeded(): Unit = {
    val es = errors
    val ws = es ++ warnings
    if (/*options.stopOnWarning &&*/ warnings.nonEmpty)
      throw CompiledUnit.Failed(this, ws)
    if (/*options.stopOnError &&*/ errors.nonEmpty)
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

  // TODO: Make this nice
  def setPostProcessingPipeline(pipeline: List[() => BaseIRVisitor]): Unit =
    this.postProcessingPipeline = pipeline
  private var postProcessingPipeline: List[() => BaseIRVisitor] = List()

  protected def printStep(title: String, content: Any): Unit =
    println(title)
    println(content)
    println()
    println("~~~~~~~~~~~~~~~~~~~~~~~")
    println()

  protected def printSteps(title: String, contents: Seq[Any]): Unit =
    println(title)
    contents.foreach { content =>
      println(content)
      println()
      println("~~~~~~~~~~~~~~~~~~~~~~~")
      println()
    }

  private lazy val loweredOtherUnits: Seq[Module] =
    val low = otherUnits.flatMap(_.lowered)
    /*val checker = typechecker
    checker.checkProgram(low)*/
    low

  lazy val lowered: Seq[Module] =
    val irLogging = compilerOptions.irLogging
    val logTyped = irLogging.logTypeInformation
    val logModule = irLogging.logModule
    val logLowerings = irLogging.logLowerings
    val logOptimizations = irLogging.logOptimizations
    val logStatsBeforeLowering = irLogging.logStatsBeforeLowering
    val logStatsBeforeOptimization = irLogging.logStatsBeforeOptimizations
    val logStatsAfterOptimization = irLogging.logStatsAfterOptimizations

    //println(s"Lower $name :: ${header.size}")
    //header.foreach(println)
    //println()

    if (logModule)
      printStep("IR-Module", if logTyped then checked else irModules)

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

    if (logStatsBeforeOptimization)
      printStatistics(loweredMods, s"before optimization")
    val p1 = optimize(loweredMods)
    if (logStatsAfterOptimization)
      printStatistics(loweredMods, s"after first optimization")
    val p2 = optimize(p1)
    if (logStatsAfterOptimization)
      printStatistics(loweredMods, s"after second optimization")

    if (logOptimizations)
      printStep(s"Optimized: ", p2)

    if isClosedWorld then
      postProcessingPipeline.foldLeft(p2) { case (ms, lowering) =>
        val lowFun = lowering()
        val ls = lowFun.visitProgram(ms)
        // Don't typecheck after postprocessing
        if (logLowerings)
          printStep(s"Post processing lowering: ${lowFun.name}", ls)
        ls
      }
    else
      p2


  def optimize(p: Seq[Module]): Seq[Module] =
    val aeval = new IRAbstractInterpreter
    aeval.evalProgram(p)
    //println("Eval module: ")
    //println(p)
    val opt = new IROptimizer(aeval)
    val po = opt.visitProgram(p)
    val checker = typechecker
    checker.checkProgram(po, header)
    po


object CompiledUnit:
  case class Failed(module: CompiledUnit, messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))
