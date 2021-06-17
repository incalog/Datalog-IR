package inca.frontend.functional.measurements.dataflow

import inca.examples.functional.ControlDataFlow
import inca.frontend.functional.executor.FunctionalExecutor.{compileFunction, loadFunction}
import inca.util.measurement.BenchmarkUtils.{Measurement, Timing, measurementsToCSV, writeFile}
import inca.util.measurement.MemoryUtil

object MeasureDataflowAnalysis extends App {
    val warmup = 10
    val numMeasurements = 20
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

    val measurements = Seq(Measurement("Powerset Dataflow Analysis", times))
    val csv = measurementsToCSV(measurements)
    println(csv)
    writeFile("benchmark/functional/powdataflow/measurements.csv", csv)
}