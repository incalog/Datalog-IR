package inca.ir

import inca.ir.extension.*
import inca.ir.lowering.BaseLowering
import inca.ir.typing.IRTypechecker
import inca.ir.util.SourceLocation
import inca.util.CompilationMessage

import scala.collection.mutable.ListBuffer

trait CompiledModule:
  def name: Name
  def sourceLocation: SourceLocation

  def ir: Module

  def printStatistics(): Unit = {
    val rels = ir.relations.values
    println(s"IR relations: ${rels.size}")
    println(s"IR bodies: ${rels.map(_.bodies.size).sum}")
  }

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
    try checker.typecheck(ir)
    finally println(ir)
    ir

  val lowerings: List[() => BaseLowering] = List(
    () => new set.Lowering {},
    () => new bool.Lowering {},
    () => new datamatch.Lowering {},
    () => new block.Lowering {},
    () => new disjunction.Lowering {},
    () => new not.Lowering {},
    () => new demand.Lowering {}
  ) // arith + string + data

  lazy val lowered: Module =
    lowerings.foldLeft(checked) { case (m, lowering) =>
      val lowFun = lowering()
      println()
      println(s"Lowering ${lowFun.loweredIRs}")
      val l = lowFun.lower(m)
      val checker = new IRTypechecker
      try checker.typecheck(l)
      finally println(l)
      l
    }

object CompiledModule:
  case class Failed(module: CompiledModule, messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))
