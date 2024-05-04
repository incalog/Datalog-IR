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
import inca.ir.valueNumbering.{ConfigVN, ValueNumbering}

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

  lazy val (checked, dependencyGraph): (Module, DependencyGraph) =
    val checker = typechecker
    checker.checkProgram(Seq(ir))
    (ir, checker.getDependencyGraph)

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
    val irLogging = compilerOptions.irLogging
    val logTyped = irLogging.logTypeInformation
    val logModule = irLogging.logModule
    val logLowerings = irLogging.logLowerings
    val logOptimizations = irLogging.logOptimizations
    val logStatsBeforeLowering = irLogging.logStatsBeforeLowering
    val logStatsBeforeOptimization = irLogging.logStatsBeforeOptimizations
    val logStatsAfterOptimization = irLogging.logStatsAfterOptimizations

    if (logModule)
      printStep("IR-Module", if (!logTyped) ir else checked)

    if (logStatsBeforeLowering)
      StatisticsCollector.printStatistics(checked, "before lowering")

    stopIfNeeded()

    val l = pipeline.foldLeft(checked) { case (m, lowering) =>
      val lowFun = lowering()
      val Seq(l) = lowFun.visitProgram(Seq(m))

      if (logLowerings && !logTyped)
        printStep(s"Lowering: ${lowFun.name}", l)

      val checker = typechecker
      try checker.checkProgram(Seq(l))
      finally if (logLowerings && logTyped)
        printStep(s"Lowering: ${lowFun.name}", l)
      l
    }

    // TODO remove other optimizations
    if (logStatsBeforeOptimization)
      printStatistics(l, s"before optimization")
    val p1 = optimize(Seq(l))
    if (logStatsAfterOptimization)
      printStatistics(p1.head, s"after first optimization")
    val p2 = optimize(p1)
    if (logStatsAfterOptimization)
      printStatistics(p2.head, s"after second optimization")

    if (logOptimizations)
      printStep(s"Optimized: ", p2)

    val p3 = valueNumbering(p2)

    postProcessingPipeline.foldLeft(p3.head) { case (m, lowering) =>
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

  
  var valueNumberingResult: Seq[Module] = Seq() // for Testing 
  def valueNumbering(p: Seq[Module], config: ConfigVN = ConfigVN(normalize=true)): Seq[Module] =
    valueNumberingResult = p.map { input =>
      valueNumbering(input,config)
    }
    valueNumberingResult

  def valueNumbering(module: Module, config: ConfigVN): Module = {
    val VN = new ValueNumbering(config)
    println(s"before VN: \n$module\n") // TODO use printstep
    val result = VN.valueNumbering(module)
    println(s"after VN: \n$result")
    result
  }




object CompiledModule:
  case class Failed(module: CompiledModule, messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))
