package inca.frontend.functional.executor.controlflow

import inca.frontend.functional.compile.{CompiledFunctionalUnit, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.frontend.functional.util.{FunctionalBenchmark, FunctionalBenchmarkConfig}
import inca.ir.execution.ThreadCount.Fixed
import inca.ir.optimize
import inca.util.FileUtil
import inca.{ascent, souffle, viatra}

import java.io.File

object ControlflowBenchmark:
  val outDir: Option[File] = Some(File("benchmark/inca_fun/ControlFlow"))

  @main
  def measureControlFlow(): Unit =
    val code = FileUtil.readFileFromResource("functional/controlflow/CFlow.finca")
    val options = FunctionalCompilerOptions.default
    val prog = nestedWhileProgram(5, 20)

    // Measure statistics exactly once
    val statConfig = FunctionalBenchmarkConfig("", "", code, FunctionalExecutor(viatra.backend.Executor()), options, optimizationPipeline = CompiledFunctionalUnit.optimizationPipeline)
    val statBenchmark = FunctionalBenchmark("ControlFlow", Seq(statConfig), "mainTransitiveFlow", Seq(prog), outDir)

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
    val benchmark = FunctionalBenchmark("ControlFlow", configs, "mainTransitiveFlow", Seq(prog), outDir)
    val (perfDs, _) = benchmark.measureAndPlotPerformance(runs = 10, warmups = 5, xLabel = Some("Engine"))
    println(perfDs.toTable)

  @main
  def measureInterval(): Unit =
    val code = FileUtil.readFileFromResource("functional/controlflow/Interval.finca")
    val options = FunctionalCompilerOptions.default
    val prog = nestedWhileProgram(5, 20)

    // Measure statistics exactly once
    val statConfig = FunctionalBenchmarkConfig("", "", code, FunctionalExecutor(viatra.backend.Executor()), options,
      optimizationPipeline = CompiledFunctionalUnit.optimizationPipeline,
      postProcessingPipeline = CompiledFunctionalUnit.viatraPostProcessingPipeline
    )
    val statBenchmark = FunctionalBenchmark("Interval", Seq(statConfig), "mainFinalVar", Seq(prog), outDir)

    val statsDs = statBenchmark.measureStatistics()
    val optimDs = statBenchmark.measureOptimizations()
    val sizeDs = statBenchmark.measureRelationStatistics()

    println(statsDs.toTable)
    println(sizeDs.toTable)
    println(optimDs.toTable)

    // Measure execution time
    val execs = Seq(
      viatra.backend.Executor(),
    )
    val configs = execs.flatMap { exec =>
      Seq(
        FunctionalBenchmarkConfig(exec.name, "unoptimized", code, FunctionalExecutor(exec), options,
          postProcessingPipeline = CompiledFunctionalUnit.viatraPostProcessingPipeline
        ),
        FunctionalBenchmarkConfig(exec.name, "optimized", code, FunctionalExecutor(exec), options,
          optimizationPipeline = CompiledFunctionalUnit.optimizationPipeline,
          postProcessingPipeline = CompiledFunctionalUnit.viatraPostProcessingPipeline
        )
      )
    }
    val benchmark = FunctionalBenchmark("Interval", configs, "mainFinalVar", Seq(prog), outDir)
    val (perfDs, _) = benchmark.measureAndPlotPerformance(runs = 10, warmups = 5, xLabel = Some("Engine"))
    println(perfDs.toTable)

  @main
  def measureControlFlowDemandStrategies(): Unit =
    val code = FileUtil.readFileFromResource("functional/controlflow/CFlow.finca")
    val options = FunctionalCompilerOptions.default
    val prog = nestedWhileProgram(5, 20)

    // Measure execution time
    val execs = Seq(
      ascent.backend.Executor(Fixed(1)),
      viatra.backend.Executor(),
      souffle.backend.Executor(Fixed(1)),
    )
    val configs = execs.flatMap { exec =>
      Seq(
        FunctionalBenchmarkConfig(exec.name, "normal", code, FunctionalExecutor(exec), options,
          pipeline = CompiledFunctionalUnit.createPipeline(false),
          optimizationPipeline = CompiledFunctionalUnit.optimizationPipeline,
        ),
        FunctionalBenchmarkConfig(exec.name, "supplementary", code, FunctionalExecutor(exec), options,
          pipeline = CompiledFunctionalUnit.createPipeline(true),
          optimizationPipeline = CompiledFunctionalUnit.optimizationPipeline,
        )
      )
    }

    val benchmark = FunctionalBenchmark("ControlFlow_demand", configs, "mainTransitiveFlow", Seq(prog), outDir)

    val statsDs = benchmark.measureStatistics()
    val optimDs = benchmark.measureOptimizations()
    val sizeDs = benchmark.measureRelationStatistics()
    val (perfDs, _) = benchmark.measureAndPlotPerformance(runs = 10, warmups = 5, xLabel = Some("Engine"))

    println(statsDs.toTable)
    println(sizeDs.toTable)
    println(optimDs.toTable)
    println(perfDs.toTable)

  @main
  def measureIntervalDemandStrategies(): Unit =
    val code = FileUtil.readFileFromResource("functional/controlflow/Interval.finca")
    val options = FunctionalCompilerOptions.default
    val prog = nestedWhileProgram(5, 20)

    // Measure execution time
    val execs = Seq(
      viatra.backend.Executor(),
    )
    val configs = execs.flatMap { exec =>
      Seq(
        FunctionalBenchmarkConfig(exec.name, "normal", code, FunctionalExecutor(exec), options,
          pipeline = CompiledFunctionalUnit.createPipeline(false),
          optimizationPipeline = CompiledFunctionalUnit.optimizationPipeline,
          postProcessingPipeline = CompiledFunctionalUnit.viatraPostProcessingPipeline
        ),
        FunctionalBenchmarkConfig(exec.name, "supplementary", code, FunctionalExecutor(exec), options,
          pipeline = CompiledFunctionalUnit.createPipeline(true),
          optimizationPipeline = CompiledFunctionalUnit.optimizationPipeline,
          postProcessingPipeline = CompiledFunctionalUnit.viatraPostProcessingPipeline
        )
      )
    }

    val benchmark = FunctionalBenchmark("Interval_demand", configs, "mainFinalVar", Seq(prog), outDir)

    val statsDs = benchmark.measureStatistics()
    val optimDs = benchmark.measureOptimizations()
    val sizeDs = benchmark.measureRelationStatistics()
    val (perfDs, _) = benchmark.measureAndPlotPerformance(runs = 10, warmups = 5, xLabel = Some("Engine"))

    println(statsDs.toTable)
    println(sizeDs.toTable)
    println(optimDs.toTable)
    println(perfDs.toTable)

  @main
  def measureIntervalIntraVsIntra(): Unit =
    def interRelationalOptimizationPipeline = List(
      () => new optimize.RemoveDuplicatedRelations {},
      () => new optimize.IRConstantOptimizer(computeControlEvents = false, interRelational = false) {},
      () => new optimize.IdentityCastElimination {},
      () => new optimize.IRConstantOptimizer(computeControlEvents = false, interRelational = true) {},
      () => new optimize.IdentityCastElimination {},
      () => new optimize.ReplaceSingletonVariables {}, // helps with detecting exact duplicates
      () => new optimize.RemoveDuplicatedRelations {},
      () => new optimize.AliasElimination {}
    )

    def intraRelationalOptimizationPipeline = List(
      () => new optimize.RemoveDuplicatedRelations {},
      () => new optimize.IRConstantOptimizer(computeControlEvents = false, interRelational = false) {},
      () => new optimize.IdentityCastElimination {},
      () => new optimize.ReplaceSingletonVariables {}, // helps with detecting exact duplicates
      () => new optimize.RemoveDuplicatedRelations {},
      () => new optimize.AliasElimination {}
    )


    val code = FileUtil.readFileFromResource("functional/controlflow/Interval.finca")
    val options = FunctionalCompilerOptions.default
    val prog = nestedWhileProgram(5, 20)

    // Measure statistics exactly once
    val statConfigs = Seq(
      FunctionalBenchmarkConfig("intra", "", code, FunctionalExecutor(viatra.backend.Executor()), options,
        optimizationPipeline = intraRelationalOptimizationPipeline,
        postProcessingPipeline = CompiledFunctionalUnit.viatraPostProcessingPipeline
      ),
      FunctionalBenchmarkConfig("inter", "", code, FunctionalExecutor(viatra.backend.Executor()), options,
        optimizationPipeline = interRelationalOptimizationPipeline,
        postProcessingPipeline = CompiledFunctionalUnit.viatraPostProcessingPipeline
      )
    )
    val statBenchmark = FunctionalBenchmark("Interval", statConfigs, "mainFinalVar", Seq(prog), outDir)

    val statsDs = statBenchmark.measureStatistics()
    val optimDs = statBenchmark.measureOptimizations()
    val sizeDs = statBenchmark.measureRelationStatistics()

    println(statsDs.toTable)
    println(sizeDs.toTable)
    println(optimDs.toTable)

  @main
  def measureControlFlowIntraVsIntra(): Unit =
    def interRelationalOptimizationPipeline = List(
      () => new optimize.RemoveDuplicatedRelations {},
      () => new optimize.IRConstantOptimizer(computeControlEvents = false, interRelational = false) {},
      () => new optimize.IdentityCastElimination {},
      () => new optimize.IRConstantOptimizer(computeControlEvents = false, interRelational = true) {},
      () => new optimize.IdentityCastElimination {},
      () => new optimize.ReplaceSingletonVariables {}, // helps with detecting exact duplicates
      () => new optimize.RemoveDuplicatedRelations {},
      () => new optimize.AliasElimination {}
    )

    def intraRelationalOptimizationPipeline = List(
      () => new optimize.RemoveDuplicatedRelations {},
      () => new optimize.IRConstantOptimizer(computeControlEvents = false, interRelational = false) {},
      () => new optimize.IdentityCastElimination {},
      () => new optimize.ReplaceSingletonVariables {}, // helps with detecting exact duplicates
      () => new optimize.RemoveDuplicatedRelations {},
      () => new optimize.AliasElimination {}
    )


    val code = FileUtil.readFileFromResource("functional/controlflow/CFlow.finca")
    val options = FunctionalCompilerOptions.default
    val prog = nestedWhileProgram(5, 20)

    // Measure statistics exactly once
    val statConfigs = Seq(
      FunctionalBenchmarkConfig("intra", "", code, FunctionalExecutor(viatra.backend.Executor()), options,
        optimizationPipeline = intraRelationalOptimizationPipeline,
        postProcessingPipeline = CompiledFunctionalUnit.viatraPostProcessingPipeline
      ),
      FunctionalBenchmarkConfig("inter", "", code, FunctionalExecutor(viatra.backend.Executor()), options,
        optimizationPipeline = interRelationalOptimizationPipeline,
        postProcessingPipeline = CompiledFunctionalUnit.viatraPostProcessingPipeline
      )
    )
    val statBenchmark = FunctionalBenchmark("ControlFlow", statConfigs, "mainTransitiveFlow", Seq(prog), outDir)

    val statsDs = statBenchmark.measureStatistics()
    val optimDs = statBenchmark.measureOptimizations()
    val sizeDs = statBenchmark.measureRelationStatistics()

    println(statsDs.toTable)
    println(sizeDs.toTable)
    println(optimDs.toTable)