package inca.ir

import inca.ir.extension.*
import inca.ir.analysis.{BaseIROptimizer, IRAbstractInterpreter, IROptimizer}
import inca.ir.lowering.BaseLowering
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker}
import inca.ir.util.SourceLocation
import inca.ir.visitors.{BaseIRVisitor, IRVisitor, StatisticsCollector}
import inca.util.{CompilationMessage, CompilerOptions}

import scala.collection.mutable.ListBuffer

trait CompiledModule(implicit val compilerOptions: CompilerOptions):
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

  def lowered: Module =
    val irOptions = compilerOptions("ir")
    val print_lowerings = irOptions.readBoolean("print_lowerings")
    val print_typed_lowerings = irOptions.readBoolean("print_typed_lowerings")
    val print_stats_before_lowering = irOptions.readBoolean("print_stats_before_lowering")
    val print_stats_before_optimization = irOptions.readBoolean("print_stats_before_optimization")
    val print_stats_after_optimization = irOptions.readBoolean("print_stats_after_optimization")

    if (print_stats_before_lowering)
      StatisticsCollector.printStatistics(checked, "before lowering")
      println()

    val l = pipeline.foldLeft(checked) { case (m, lowering) =>
      val lowFun = lowering()
      val Seq(l) = lowFun.visitProgram(Seq(m))

      if (print_lowerings)
        println(s"Lowering: ${lowFun.name}")
        println(l)
        println()

      val checker = typechecker
      checker.checkModule(l)

      if (print_typed_lowerings)
        println(s"Typed Lowering: ${lowFun.name}")
        println(l)
        println()
      l
    }

    if (print_stats_before_optimization)
      printStatistics(l, s"before optimization")
      println()
    val p1 = optimize(Seq(l))
    if (print_stats_after_optimization)
      printStatistics(l, s"before optimization")
      println()
    val p2 = optimize(p1)
    if (print_stats_after_optimization)
      printStatistics(l, s"before optimization")
      println()

    postProcessingPipeline.foldLeft(p2.head) { case (m, lowering) =>
      val lowFun = lowering()
      val Seq(l) = lowFun.visitProgram(Seq(m))
      // Don't typecheck after postprocessing
      if (print_lowerings)
        println(s"Post processing lowering: ${lowFun.name}")
        println(l)
        println()
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
