package inca.ir

import inca.ir
import inca.ir.util.SourceLocation
import inca.ir.visitors.BaseIRVisitor
import inca.util.compileroptions.CompilerOptions
import inca.ir.extension.*

object CompiledTestUnit:
  val pipeline: List[() => BaseIRVisitor] = List(
    () => new typeparam.Lowering {},
    () => new aggregateset.Lowering {},
    () => new set.Lowering {},
    () => new map.Lowering {},
    () => new bool.Lowering {},
    () => new datamatch.Lowering {},
    () => new block.Lowering {},
    () => new disjunction.Lowering {},
    () => new not.Lowering {},
    () => new demand.Lowering {},
    () => new tuple.Lowering {},
  ) // arith + string + data

case class CompiledTestUnit(mod: ir.Module) extends CompiledUnit:

  setPipeline(CompiledTestUnit.pipeline)

  override def compilerOptions: CompilerOptions = CompilerOptions.default
  override def name: Name = mod.name
  override def sourceLocation: SourceLocation = mod.name
  override def isClosedWorld: Boolean = true
  override def irModules: Seq[Module] = Seq(mod)
  override def otherUnits: Seq[CompiledUnit] = Seq()


