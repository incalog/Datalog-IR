package inca.casestudy.set

import inca.casestudy.util.Util.{collectGarbage, toCSV}
import inca.ir.*
import inca.ir.execution.ThreadCount.Fixed
import inca.ir.execution.{IRExecutor, UnitRelation}
import inca.ir.extension.*
import inca.ir.extension.arithmetic.*
import inca.ir.extension.demand.*
import inca.ir.extension.set
import inca.ir.extension.set.SyntacticOptimizer
import inca.ir.optimize.AliasElimination
import inca.ir.util.SourceLocation
import inca.util.CSVUtil.{CSV, csvToString}
import inca.util.FileUtil
import inca.util.compileroptions.CompilerOptions
import inca.viatra.backend.Executor

import scala.language.implicitConversions

def factorial(n: Int) = (1 to n).product

object SetUnion:
  val n = 6 // choose a big enough number
  val combinations = 2*factorial(n)
  println(s"Number of unique sets: $combinations")

  val a = Range(1, n+1).map(arithmetic.IntNum.apply)
  val b = Range(n+1, (n*2)+1).map(arithmetic.IntNum.apply)

  def main(numSets: Int) = Relation("main",
    Seq(
      Param("s", set.TSet(arithmetic.TInt)),
      Param("v", arithmetic.TInt),
    ),
    Seq(
      Body(Seq(
        Eq(
          Var("s"),
          // We represent sets with case classes, that is make them unique by permutation
          set.SetUnion(a.permutations.zip(b.permutations).flatten {case (a,b) => Seq(a,b) }.toSeq.take(numSets).map(set.SetLit.apply))
        ),
        set.SetMember(Var("v"), Var("s"))
      ))
    )
  )


  def createMod(numSets: Int) = Module("SetUnionMicro", BaseIR.language + arithmetic.IR + set.IR,
    Seq(
      main(numSets),
    )
  )


  def createCompiled(mod: Module, optimizeSets: Boolean): CompiledModule = new CompiledModule:
    override def name: Name = mod.name
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
    override def ir: Module = mod
    override def compilerOptions: CompilerOptions = {
      val opt = CompilerOptions.default
      opt.irLogging.logModule = false
      opt.irLogging.logLowerings = false
      opt.irLogging.logTypeInformation = false
      opt.irLogging.logStatsAfterOptimizations = false
      opt
    }

    val optim = if optimizeSets then
      List(() => new SyntacticOptimizer {})
    else
      List()

    setPipeline(optim ++ List(
      () => new set.Lowering {},
      () => new block.Lowering {},
      () => new disjunction.Lowering {},
      () => new demand.Lowering {},
      () => new tuple.Lowering {},
      () => new AliasElimination {}
    ))


  private def run(executor: IRExecutor) = {
    // combinations + 1
    val step = 20
    for (i <- Range(2, 200+step, step)) {
      val mod = createMod(i)
      val compiled = createCompiled(mod, false)
      val engine = executor.instantiate(compiled)
      //val rel = engine.read(UnitRelation("main"))
      //println(rel.asTable)
      val diff = engine.measure(UnitRelation("main"))
      println(s"Set union of: ${i} - ${diff}")
    }
  }

  @main def runSetUnionUsingViatra() = {
    run(inca.viatra.backend.Executor())
  }

  @main def runSetUnionUsingSouffle() = {
    run(inca.souffle.backend.Executor(Fixed(1)))
  }

  @main def runSetUnionUsingAscent() = {
    run(inca.ascent.backend.Executor(Fixed(1)))
  }

  