package inca.ir.extension.monotypes

import inca.ir.util.SourceLocation
import inca.ir.{CompiledModule, Name}
import inca.ir.Module
import inca.ir.extension.{aggregateset, block, bool, impure, demand, disjunction, not, set, tuple}
import inca.ir.visitors.BaseIRVisitor

case class CompiledMonoModule(mod: Module) extends CompiledModule:
  override def name: Name = mod.name

  override def sourceLocation: SourceLocation = mod.name

  override def ir: Module = mod

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
