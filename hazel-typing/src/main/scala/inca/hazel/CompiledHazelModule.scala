package inca.hazel

import inca.ir.*
import inca.ir.extension.{aggregateset, block, bool, datamatch, demand, disjunction, map, not, set, tuple, typeparam}
import inca.ir.util.SourceLocation
import inca.ir.visitors.BaseIRVisitor
import inca.util.compileroptions.CompilerOptions

class CompiledHazelModule(val ir: Module) extends CompiledModule:
  override def name: Name = ir.name
  override def sourceLocation: SourceLocation = ir
  override def compilerOptions: CompilerOptions = CompilerOptions.default

  setPipeline(List(
    () => new typeparam.Lowering {},
    () => new aggregateset.Lowering {},
    () => new set.Lowering {},
    () => new map.Lowering {},
    () => new bool.Optimizer {},
    () => new bool.Lowering {},
    () => new datamatch.Lowering {},
    () => new block.Lowering {},
    () => new disjunction.Lowering {},
    () => new not.Lowering {},
    () => new demand.Lowering {},
    () => new tuple.Lowering {},
  ))
