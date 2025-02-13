package inca.frontend.functional.executor.asg

import inca.frontend.functional.compile.{CompiledFunctionalUnit, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.frontend.functional.util.{FunctionalBenchmark, FunctionalBenchmarkConfig}
import inca.ir.execution.ThreadCount.Fixed
import inca.util.FileUtil
import inca.{ascent, souffle, viatra}

import java.io.File

// TODO: Use a bigger input program
object AsgBenchmark:
  val outDir: Option[File] = Some(File("/Users/David/Desktop"))

  @main
  def measureAsg(): Unit =
    val code = FileUtil.readFileFromResource("functional/asg/DependencyAnalysis.finca")
    val options = FunctionalCompilerOptions.default
    val prog = prog1

    // Measure statistics exactly once
    val statConfig = FunctionalBenchmarkConfig(s"-", code, FunctionalExecutor(viatra.backend.Executor()), options, optimizationPipeline = CompiledFunctionalUnit.optimizationPipeline)
    val statBenchmark = FunctionalBenchmark("ASG", Seq(statConfig), "main", Seq(prog), outDir)

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
        FunctionalBenchmarkConfig(s"${exec.name}", code, FunctionalExecutor(exec), options),
        FunctionalBenchmarkConfig(s"${exec.name} (opt)", code, FunctionalExecutor(exec), options, optimizationPipeline = CompiledFunctionalUnit.optimizationPipeline)
      )
    }
    val benchmark = FunctionalBenchmark("ASG", configs, "main", Seq(prog), outDir)
    val perfDs = benchmark.measureAndPlotPerformance(runs = 5, warmups = 3)
    println(perfDs.toTable)