package inca.frontend.functional.measurements.itypes

import inca.frontend.functional.executor.{FunctionalExecutor, IncrementalFunctionalExecutor}
import inca.util.FileUtil
import inca.util.measurement.BenchmarkUtils.{Measurement, Timing, measurementsToCSV, writeFile}
import inca.util.measurement.MemoryUtil

import scala.collection.mutable
import scala.meta.{XtensionParseInputLike, XtensionQuasiquoteTerm}


// set -Xss1G otherwise scalameta parse throws stackoverflow
// set -Xmx16G to give as much as heap memory as possible
object PerformMeasurements extends scala.App {
  val code = FileUtil.readFile("functional/itypes/TypeChecker.finca")

  // generate measurement configs
  val warmupMeasurments = 10
  val numMeasurments = 40
  implicit val timing: Timing = Timing(warmupMeasurments, numMeasurments)
  val configs = MeasurementConfig.generate(200, warmupMeasurments, numMeasurments)

  // measure initialization times
  val starDependencyConfig = configs.find(_.gen.isInstanceOf[GenerateStarDependencyProg.type]).get
  val chainDependencyConfig = configs.find(_.gen.isInstanceOf[GenerateChainDependencyProg.type]).get

  val initMeasurements = Seq(starDependencyConfig, chainDependencyConfig).map { config =>
    val prog = config.gen.generate(config.depth)
    val emptyCtx = q"Empty()"

    val initialTimes = (0 until config.warmupMeasurements + config.numMeasurements).map { _ =>
      // load analysis
      val analysis = FunctionalExecutor.loadFunction(code)

      // collect garbage before running analysis
      MemoryUtil.collectGarbage()

      // initialize analysis
      val (loadTime, initialQueryTime,_) = analysis.measure("typeOf", Seq(emptyCtx, toScalaMeta(prog)))
      initialQueryTime
    }

    val baseConfigName = config.gen.getClass.getSimpleName.replaceAllLiterally("$", "") + " Initial"
    Measurement(baseConfigName, initialTimes)
  }

  // measure incremental update times

  val incrementalMeasurements = configs.flatMap { config =>
    // generate program and edit
    val prog = config.gen.generate(config.depth)
    val progEdit =  config.edit.edit(prog)
    val emptyCtx = q"Empty()"

    // load analysis
    val analysis = IncrementalFunctionalExecutor.loadFunction(code)

    // collect garbage before running analysis
    MemoryUtil.collectGarbage()

    // initialize analysis
    analysis.measureInitial("typeOf", Seq(emptyCtx, toScalaMeta(prog)))

    val baseConfigName = config.gen.getClass.getSimpleName.replaceAllLiterally("$", "") + " " + config.edit.getClass.getSimpleName.replaceAllLiterally("$", "")

    val editTimes = mutable.ListBuffer[(Long, Long, Long)]()
    val undoTimes = mutable.ListBuffer[(Long, Long, Long)]()
    // do measurements
    (0 until config.warmupMeasurements + config.numMeasurements).foreach { _ =>
      val editTime = analysis.measureUpdate("typeOf", Seq(emptyCtx, toScalaMeta(progEdit)))
      editTimes += ((editTime._1, editTime._2, editTime._3))
      val undoTime = analysis.measureUpdate("typeOf", Seq(emptyCtx, toScalaMeta(prog)))
      undoTimes +=  ((undoTime._1, undoTime._2, undoTime._3))
    }

    val editMeasurement = Measurement(baseConfigName + " Edit", editTimes.map(_._2).toSeq)
    val undoMeasurement = Measurement(baseConfigName + " Undo", undoTimes.map(_._2).toSeq)
    val combinedMeasurement = editMeasurement.combine(baseConfigName + " Edit + Undo", undoMeasurement)
    Seq(editMeasurement, undoMeasurement, combinedMeasurement)
  }
  val allMeasurements = initMeasurements ++ incrementalMeasurements
  println(measurementsToCSV(allMeasurements))
  writeFile("benchmark/itypes/measurements.csv", measurementsToCSV(allMeasurements))

  def toScalaMeta(exp: Exp): meta.Term = {
    exp.toString.parse[meta.Term].get
  }
}