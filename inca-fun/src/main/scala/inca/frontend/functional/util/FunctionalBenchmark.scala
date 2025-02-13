package inca.frontend.functional.util

import benchmark.util.TimeUnit.Second
import benchmark.util.{Dataset, TimeUnit}
import inca.frontend.functional.compile.GenerateIR.extensionalRelationName
import inca.frontend.functional.compile.{CompiledFunctionalUnit, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.ir.execution.{ExecutorEngine, IRExecutor, Relation, UnitRelation}
import inca.ir.optimize.Optimizer
import inca.ir.visitors.BaseIRVisitor
import inca.util.{Benchmark, BenchmarkConfig}

import java.io.File

case class FunctionalBenchmarkConfig(override val name: String,
                                     code: String,
                                     exec: FunctionalExecutor,
                                     options: FunctionalCompilerOptions = FunctionalCompilerOptions.default,
                                     pipeline: List[() => BaseIRVisitor] = CompiledFunctionalUnit.pipeline,
                                     optimizationPipeline: List[() => Optimizer] = List()) extends BenchmarkConfig:
  override def toString: String = name


case class FunctionalBenchmark(override val name: String,
                               override val configs: Seq[FunctionalBenchmarkConfig],
                               main: String,
                               args: Seq[Any],
                               override val outDir: Option[File] = None) extends Benchmark[FunctionalBenchmarkConfig]:

  override def newInstantiatedExecutorEngine(config: FunctionalBenchmarkConfig): ExecutorEngine =
    val compiled = config.exec.compileFunction(config.code, config.options)
    compiled.setPipeline(config.pipeline)
    compiled.setOptimizationPipeline(config.optimizationPipeline)
    val loaded = config.exec.loadFunction(compiled)
    val engine = loaded.engine
    if (args.nonEmpty)
      val edbEntry = Relation.from(extensionalRelationName(main), args)
      engine.insert(edbEntry)
    engine

  def measurePerformance(runs: Int, warmups: Int): Dataset =
    measurePerformance(UnitRelation(main), runs, warmups)

  def measureAndPlotPerformance(runs: Int, warmups: Int, title: String, outdir: Option[File] = None, timeUnit: TimeUnit = Second, openPlot: Boolean = true): Unit =
    val ds = measurePerformance(runs, warmups)
    boxPlotPerformance(ds, timeUnit, openPlot)