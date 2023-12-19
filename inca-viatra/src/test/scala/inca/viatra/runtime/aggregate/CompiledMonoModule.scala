package inca.viatra.runtime.aggregate

import inca.foreign.scala.analysis.{ScalaAbstractInterpreter, ScalaIROptimizer}
import inca.ir.extension.*
import inca.ir.extension.mono.Lowering
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker}
import inca.ir.util.SourceLocation
import inca.ir.visitors.{BaseIRVisitor, StatisticsCollector}
import inca.ir.{Atom, CompiledModule, Module, Name}
import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.primitive.Visitor as ScalaVisitor
import inca.foreign.scala.visitors.ScalaStatisticsCollector
import inca.ir.extension.impure.CollectImpurityKinds
import inca.util.Implicits._

case class CompiledMonoModule(mod: Module) extends CompiledModule:
  override def name: Name = mod.name

  override def sourceLocation: SourceLocation = mod.name

  override def ir: Module = mod

  private class MonoTypeChecker extends IRTypechecker with primitive.Typechecker

  override def typechecker: BaseIRTypechecker = new MonoTypeChecker()

  override def printStatistics(module: Module, str: String): Unit =
    ScalaStatisticsCollector.printStatistics(module, str)


  override def optimize(p: Seq[Module]): Seq[Module] =
    val aeval = new ScalaAbstractInterpreter
    aeval.evalModule(p.head)
    val opt = new ScalaIROptimizer(aeval)
    val po = opt.visitProgram(p)
    val checker = typechecker
    checker.checkProgram(po)
    po


object CompiledMonoModule:
  trait ScalaImpKindsCollector extends CollectImpurityKinds with ScalaVisitor
  val pipeline: List[() => BaseIRVisitor] = List(
    () => new Lowering {},
    () => new impure.Lowering {},
    () => new aggregateset.Lowering {},
    () => new set.Lowering {},
    () => new bool.Lowering {},
    () => new block.Lowering {},
    () => new disjunction.Lowering {},
    () => new not.Lowering {},
    () => new demand.Lowering {},
    () => new tuple.Lowering {},
  ) // arith + string + data
