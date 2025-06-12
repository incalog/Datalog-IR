package inca.frontend.oodl.util

import benchmark.util.TimeUnit.Second
import benchmark.util.{Dataset, TimeUnit}
import inca.frontend.oodl.compile.GenerateIR.extensionalRelationName
import inca.frontend.oodl.compile.{CompiledOODLUnit, OODLCompilerOptions}
import inca.frontend.oodl.executor.OODLExecutor
import inca.ir.execution.{ExecutorEngine, Relation, UnitRelation}
import inca.ir.optimize.Optimizer
import inca.ir.visitors.BaseIRVisitor
import inca.util.{Benchmark, BenchmarkConfig}

import java.io.File

val DELIMITER = " - "

case class OODLBenchmarkConfig(category: String,
                               group: String,
                               code: String,
                               exec: OODLExecutor,
                               options: OODLCompilerOptions = OODLCompilerOptions.default,
                               pipeline: List[() => BaseIRVisitor] = CompiledOODLUnit.pipeline,
                               optimizationPipeline: List[() => Optimizer] = List(),
                               includePostProcessingPipeline: Boolean = false
                              ) extends BenchmarkConfig:
  override val name = s"$category$DELIMITER$group"
  override def toString: String = name


case class OODLBenchmark(override val name: String,
                         override val configs: Seq[OODLBenchmarkConfig],
                         main: String,
                         args: Seq[Any], // TODO: Move the args to the config
                         override val outDir: Option[File] = None)
  extends Benchmark[OODLBenchmarkConfig, CompiledOODLUnit]:

  override def setupCompiledUnit(config: OODLBenchmarkConfig): CompiledOODLUnit =
    val compiled = config.exec.compileOODL(config.code, config.options)
    compiled.setPipeline(config.pipeline)
    compiled.setOptimizationPipeline(config.optimizationPipeline)
    if (config.includePostProcessingPipeline)
      compiled.setPostProcessingPipeline(compiled.viatraPostProcessingPipeline)
    compiled

  override def setupEngine(config: OODLBenchmarkConfig): ExecutorEngine =
    val compiled = setupCompiledUnit(config)
    val loaded = config.exec.loadOODL(compiled)
    val engine = loaded.engine
    val allocIn = 1
    val mutIn = 1
    val monoIn = 1
    val edbEntry = Relation.from(extensionalRelationName(main), args :+ allocIn :+ mutIn :+ monoIn)
    engine.insert(edbEntry)
    engine

  def measurePerformance(runs: Int, warmups: Int, topK: Int = 3): Dataset =
    measurePerformance(UnitRelation(main), runs, warmups, topK)

  def measureAndPlotPerformance(runs: Int, warmups: Int, timeUnit: TimeUnit = Second, openPlot: Boolean = true, xLabel: Option[String] = None): (Dataset, File) =
    val ds = measurePerformance(runs, warmups)
    val imgFile = groupedBarPlotPerformance(ds, timeUnit, DELIMITER, openPlot, xLabel)
    (ds, imgFile)