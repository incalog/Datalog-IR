package inca.frontend.functional.dataflow

import inca.examples.functional.ControlDataFlow
import inca.frontend.functional.executor.FunctionalExecutor.compileFunction
import inca.frontend.functional.executor.FunctionalExecutor.loadFunction
import inca.measurements.util.BenchmarkUtils.measurementsToCSV
import inca.measurements.util.BenchmarkUtils.Measurement
import inca.measurements.util.BenchmarkUtils.Timing
import inca.measurements.util.MemoryUtil
import inca.measurements.util.Units
import inca.util.FilesUtil

object MeasureDataflowAnalysis extends App {
  val warmup = 0
  val numMeasurements = 100
  implicit val timing: Timing = Timing(warmup, numMeasurements)

  val compiled = compileFunction(ControlDataFlow.IntValuesModule)
  val times = (0 until timing.discard + timing.repeat).map { _ =>
    // load analysis
    val fun = loadFunction(compiled)

    // collect garbage before running analysis
    MemoryUtil.collectGarbage()

    // initialize analysis
    val (_, time, _) = fun.measure("final_var", Seq(ControlDataFlow.exampleDataflow2))
    println(time)
    time
  }

  val measurements = Seq(Measurement("Powerset Dataflow Analysis", Units.Nanoseconds, times))
  val csv = measurementsToCSV(measurements)
  println(csv)
  FilesUtil.writeFile("benchmark-results/functional/powdataflow/measurements.csv", csv)
}
