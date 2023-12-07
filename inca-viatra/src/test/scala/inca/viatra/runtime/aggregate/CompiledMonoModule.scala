package inca.viatra.runtime.aggregate

import inca.ir.extension.*
import inca.ir.extension.mono.Lowering
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker}
import inca.ir.util.SourceLocation
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{CompiledModule, Module, Name}
import inca.foreign.scala.ir.primitive

case class CompiledMonoModule(mod: Module) extends CompiledModule:
  override def name: Name = mod.name

  override def sourceLocation: SourceLocation = mod.name

  override def ir: Module = mod

  private class MonoTypeChecker extends IRTypechecker with primitive.Typechecker

  override def typechecker: BaseIRTypechecker = new MonoTypeChecker()

object CompiledMonoModule:
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
