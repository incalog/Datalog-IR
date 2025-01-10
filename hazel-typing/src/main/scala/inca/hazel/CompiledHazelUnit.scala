package inca.hazel

import inca.ir.*
import inca.ir.optimize.{IRConstantOptimizer, AliasElimination}
import inca.ir.extension.{aggregateset, block, bool, datamatch, demand, disjunction, map, not, set, tuple, typeparam}
import inca.ir.util.SourceLocation
import inca.ir.visitors.BaseIRVisitor
import inca.util.compileroptions.CompilerOptions

class CompiledHazelUnit(val ir: Module) extends CompiledUnit:
  override def name: Name = ir.name

  override def sourceLocation: SourceLocation = ir

  override def compilerOptions: CompilerOptions = CompilerOptions.default

  override def irModules: Seq[Module] = Seq(ir)

  override val isClosedWorld: Boolean = true

  override def otherUnits: Seq[CompiledUnit] = Seq()

  setPipeline(List(
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
    () => new tuple.Lowering {}
  ))

  val optimizations: List[() => BaseIRVisitor] = List(
    //() => new optimize.TypeIROptimizer {},
    () => new IRConstantOptimizer(assumeEdbIsNotEmpty = true, computeControlEvents = false) {},
    () => new IRConstantOptimizer(assumeEdbIsNotEmpty = true, computeControlEvents = true) {},
    () => new AliasElimination {}
  )
  setOptimizationPipeline(optimizations)
