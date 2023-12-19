package inca.ir

import inca.ir.extension.*
import inca.ir.analysis.{BaseIROptimizer, IRAbstractInterpreter, IROptimizer}
import inca.ir.lowering.BaseLowering
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker}
import inca.ir.util.SourceLocation
import inca.ir.visitors.{BaseIRVisitor, IRVisitor, StatisticsCollector}
import inca.util.CompilationMessage
import inca.util.compileroptions.CompilerOptions

import scala.collection.mutable.ListBuffer

trait CompiledModule:
  def compilerOptions: CompilerOptions
  def name: Name
  def sourceLocation: SourceLocation

  def ir: Module

  protected val messages: ListBuffer[CompilationMessage] = ListBuffer()
  def allMessages: List[CompilationMessage] = messages.toList
  def errors: List[CompilationMessage] = messages.filter(_.severity == CompilationMessage.ERROR).toList
  def warnings: List[CompilationMessage] = messages.filter(_.severity == CompilationMessage.WARNING).toList

  protected def stopIfNeeded(): Unit = {
    val es = errors
    val ws = es ++ warnings
    if (/*options.stopOnWarning &&*/ warnings.nonEmpty)
      throw CompiledModule.Failed(this, ws)
    if (/*options.stopOnError &&*/ errors.nonEmpty)
      throw CompiledModule.Failed(this, es)
  }

  protected def typechecker: BaseIRTypechecker = new IRTypechecker
  
  protected def printStatistics(module: Module, str: String): Unit =
    StatisticsCollector.printStatistics(module, str)

  lazy val checked: Module =
    val checker = typechecker
    checker.checkModule(ir)
    ir

  // TODO should be configurable
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

  def lowered: Module =
    val irLogging = compilerOptions("ir_logging")
    val logTyped = irLogging.readBoolean("typed")
    val logModule = irLogging.readBoolean("module")
    val logLowerings = irLogging.readBoolean("lowerings")
    val logStatsBeforeLowering = irLogging.readBoolean("stats_before_lowering")
    val logStatsBeforeOptimization = irLogging.readBoolean("stats_before_optimization")
    val logStatsAfterOptimization = irLogging.readBoolean("stats_after_optimization")

    if (logModule)
      printStep("Module", if (!logTyped) ir else checked)

    if (logStatsBeforeLowering)
      StatisticsCollector.printStatistics(checked, "before lowering")

    val l = pipeline.foldLeft(checked) { case (m, lowering) =>
      val lowFun = lowering()
      val Seq(l) = lowFun.visitProgram(Seq(m))

      if (logLowerings && !logTyped)
        printStep(s"Lowering: ${lowFun.name}", l)

      val checker = typechecker
      checker.checkModule(l)

      if (logLowerings && logTyped)
        printStep(s"Lowering: ${lowFun.name}", l)
      l
    }

    if (logStatsBeforeOptimization)
      printStatistics(l, s"before optimization")
    val p1 = optimize(Seq(l))
    if (logStatsAfterOptimization)
      printStatistics(l, s"before optimization")
    val p2 = optimize(p1)
    if (logStatsAfterOptimization)
      printStatistics(l, s"before optimization")

    postProcessingPipeline.foldLeft(p2.head) { case (m, lowering) =>
      val lowFun = lowering()
      val Seq(l) = lowFun.visitProgram(Seq(m))
      // Don't typecheck after postprocessing
      if (logLowerings)
        printStep(s"Post processing lowering: ${lowFun.name}", l)
      l
    }

  def optimize(p: Seq[Module]): Seq[Module] =
    val aeval = new IRAbstractInterpreter
    aeval.evalModule(p.head)
    //println("Eval module: ")
    //println(p)
    val opt = new IROptimizer(aeval)
    val po = opt.visitProgram(p)
    val checker = typechecker
    checker.checkProgram(po)
    po


object CompiledModule:
  case class Failed(module: CompiledModule, messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))
