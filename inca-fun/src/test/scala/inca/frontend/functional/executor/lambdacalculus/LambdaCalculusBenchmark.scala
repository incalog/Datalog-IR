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
  val outDir: Option[File] = Some(File("/Users/David/Desktop"))

  @main
  def measureLambdaCalculus(): Unit =
     val code = FileUtil.readFileFromResource("functional/lambdacalculus/LambdaCalculus.finca")
      val execs = Seq(
        ascent.backend.Executor(Fixed(1)),
        viatra.backend.Executor(),
        souffle.backend.Executor(Fixed(1)),
      )
      val options = FunctionalCompilerOptions.default
      val configs = execs.flatMap { exec =>
        Seq(
          FunctionalBenchmarkConfig(s"${exec.name}", code, FunctionalExecutor(exec), options),
          FunctionalBenchmarkConfig(s"${exec.name} (opt)", code, FunctionalExecutor(exec), options, optimizationPipeline = CompiledFunctionalUnit.optimizationPipeline)
        )
      }
      val prog = generateTypedProg(50)
      val benchmark = FunctionalBenchmark("LambdaCalculus", configs, "main", Seq(prog), outDir)

      val sizeDs = benchmark.measureTuples()
      println(sizeDs.toTable)

      benchmark.measureAndPlotPerformance(1, 0, "Lambda Calculus")