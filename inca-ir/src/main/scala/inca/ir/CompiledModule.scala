package inca.ir

import inca.ir.extension.*
import inca.ir.analysis.{BaseIROptimizer, IRAbstractInterpreter, IROptimizer}
import inca.ir.lowering.BaseLowering
import inca.ir.typing.IRTypechecker
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

  lazy val checked: Module =
    val checker = new IRTypechecker
    checker.typecheck(ir)
    ir

  val pipeline: List[() => BaseIRVisitor] = List(
    () => new aggregateset.Lowering {},
    () => new set.Lowering {},
    () => new bool.Lowering {},
    () => new datamatch.Lowering {},
    () => new block.Lowering {},
    () => new disjunction.Lowering {},
    () => new not.Lowering {},
    () => new demand.Lowering {},
    () => new tuple.Lowering {},
  ) // arith + string + data

  lazy val lowered: Module =
    StatisticsCollector.printStatistics(checked, "before lowering")
    val l = pipeline.foldLeft(checked) { case (m, lowering) =>
      val lowFun = lowering()
      val Seq(l) = lowFun.visitProgram(Seq(m))
      val checker = new IRTypechecker
      checker.typecheck(l)
      l
    }

    StatisticsCollector.printStatistics(l, s"before optimization")
    val p1 = optimize(Seq(l))
    StatisticsCollector.printStatistics(p1.head, s"after optimization 1")
    val p2 = optimize(p1)
    StatisticsCollector.printStatistics(p2.head, s"after optimization 2")
    println(p2)

    p2.head

  def optimize(p: Seq[Module]): Seq[Module] =
    val aeval = new IRAbstractInterpreter
    aeval.evalModule(p.head)
    println(p.head)
    val opt = new IROptimizer(aeval)
    val po = opt.visitProgram(p)
    val checker = new IRTypechecker
    checker.typecheck(po)
    po


object CompiledModule:
  case class Failed(module: CompiledModule, messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))
