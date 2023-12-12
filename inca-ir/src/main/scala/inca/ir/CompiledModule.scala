package inca.ir

import inca.ir.extension.*
import inca.ir.analysis.{BaseIROptimizer, IRAbstractInterpreter, IROptimizer}
import inca.ir.lowering.BaseLowering
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker}
import inca.ir.util.SourceLocation
import inca.ir.visitors.{BaseIRVisitor, IRVisitor, StatisticsCollector}
import inca.util.CompilationMessage

import scala.collection.mutable.ListBuffer

trait CompiledModule:
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
    StatisticsCollector.printStatistics(checked, "before lowering")
    //println()
    //println(checked)
    //println()
    var i = 0
    val l = pipeline.foldLeft(checked) { case (m, lowering) =>
      val lowFun = lowering()
      val Seq(l) = lowFun.visitProgram(Seq(m))

      val checker = typechecker
      checker.checkModule(l)

//      println(s"Lowering $i")
//      println(l)
//      println()

      /*println(s"Checked $i")
      println(l)
      println()*/
      i += 1
      l
    }

//    printStatistics(l, s"before optimization")
//    val p1 = optimize(Seq(l))
//    printStatistics(p1.head, s"after optimization 1")
//    val p2 = optimize(p1)
//    printStatistics(p2.head, s"after optimization 2")

    postProcessingPipeline.foldLeft(p2.head) { case (m, lowering) =>
      val lowFun = lowering()
      val Seq(l) = lowFun.visitProgram(Seq(m))
      // Don't typecheck after postprocessing
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
