package inca.frontend.functional.executor.lambdacalculus

import inca.frontend.functional.util.{FunctionalBenchmark, FunctionalBenchmarkConfig}
import inca.util.FileUtil
import inca.ascent
import inca.frontend.functional.compile.{CompiledFunctionalUnit, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.ir.execution.ThreadCount.Fixed
import inca.souffle
import inca.viatra

import java.io.File

object LambdaCalculusBenchmark:
  val outDir: Option[File] = Some(File("benchmark/inca_fun/LambdaCalculus"))

  @main
  def measureLambdaCalculus(): Unit =
    val code = FileUtil.readFileFromResource("functional/lambdacalculus/LambdaCalculus.finca")
    val options = FunctionalCompilerOptions.default
    val prog = generateTypedProg(50)

    // Measure statistics exactly once
    val statConfig = FunctionalBenchmarkConfig("", "", code, FunctionalExecutor(viatra.backend.Executor()), options, optimizationPipeline = CompiledFunctionalUnit.optimizationPipeline)
    val statBenchmark = FunctionalBenchmark("LambdaCalculus", Seq(statConfig), "main", Seq(prog), outDir)

    val statsDs = statBenchmark.measureStatistics()
    val optimDs = statBenchmark.measureOptimizations()
    val sizeDs = statBenchmark.measureRelationStatistics()

    println(statsDs.toTable)
    println(sizeDs.toTable)
    println(optimDs.toTable)

    // Measure execution time
    val execs = Seq(
      ascent.backend.Executor(Fixed(1)),
      viatra.backend.Executor(),
      souffle.backend.Executor(Fixed(1)),
    )
    val configs = execs.flatMap { exec =>
      Seq(
        FunctionalBenchmarkConfig(exec.name, "unoptimized", code, FunctionalExecutor(exec), options),
        FunctionalBenchmarkConfig(exec.name, "optimized", code, FunctionalExecutor(exec), options, optimizationPipeline = CompiledFunctionalUnit.optimizationPipeline)
      )
    }
    val benchmark = FunctionalBenchmark("LambdaCalculus", configs, "main", Seq(prog), outDir)
    val (perfDs, _) = benchmark.measureAndPlotPerformance(runs = 10, warmups = 5, xLabel = Some("Engine"))
    println(perfDs.toTable)

  @main
  def measureLambdaCalculusDemandStrategies(): Unit =
    val code = FileUtil.readFileFromResource("functional/lambdacalculus/LambdaCalculus.finca")
    val options = FunctionalCompilerOptions.default
    val prog = generateTypedProg(50)

    // Measure execution time
    val execs = Seq(
      ascent.backend.Executor(Fixed(1)),
      viatra.backend.Executor(),
      souffle.backend.Executor(Fixed(1)),
    )
    val configs = execs.flatMap { exec =>
      Seq(
        FunctionalBenchmarkConfig(exec.name, "normal", code, FunctionalExecutor(exec), options, pipeline = CompiledFunctionalUnit.createPipeline(false), optimizationPipeline = CompiledFunctionalUnit.optimizationPipeline),
        FunctionalBenchmarkConfig(exec.name, "supplementary", code, FunctionalExecutor(exec), options, pipeline = CompiledFunctionalUnit.createPipeline(true), optimizationPipeline = CompiledFunctionalUnit.optimizationPipeline)
      )
    }
    val benchmark = FunctionalBenchmark("LambdaCalculus_demand", configs, "main", Seq(prog), outDir)
    val statsDs = benchmark.measureStatistics()
    val optimDs = benchmark.measureOptimizations()
    val sizeDs = benchmark.measureRelationStatistics()
    val (perfDs, _) = benchmark.measureAndPlotPerformance(runs = 10, warmups = 5, xLabel = Some("Engine"))

    println(perfDs.toTable)
    println(statsDs.toTable)
    println(sizeDs.toTable)
    println(optimDs.toTable)