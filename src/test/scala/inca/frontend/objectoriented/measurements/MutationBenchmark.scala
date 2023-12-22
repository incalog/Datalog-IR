package inca.frontend.objectoriented.measurements

import inca.backend.transform.Transformation
import inca.backend.transform.objectoriented.{EclipseStructuralMutationTransformation, NumericMutationTransformation, StructuralMutationTransformation}
import inca.compiler.Compiler
import inca.frontend.ir.Relation
import inca.frontend.objectoriented.compiler.ObjectOptions
import inca.frontend.objectoriented.datalog.ObjectOrientedDatalog
import inca.frontend.objectoriented.interpreter._
import inca.frontend.objectoriented.parser.Parser
import inca.frontend.objectoriented.transformations.{AddMissingDefinitions, InsertBuiltInMonotones}
import inca.frontend.objectoriented.typechecker.Typechecker
import inca.runtime.EnginePool
import inca.util.FileUtil
import inca.util.measurement.CSVUtil.{CSV, csvToString}
import inca.util.measurement.MemoryUtil

import scala.meta.Term

case class MutationBenchmark(warmups: Int, runs: Int) {
  val progFolder: String = s"objectoriented/measurements/"
  val resultPath: String = "benchmark/objectoriented"

  trait Config {
    val warmup: Int
    val runs: Int
    val name: String
  }
  case class MutationConfig(warmup: Int, runs: Int, name: String, numNodes: Int, numMutations: Int) extends Config {
    override def toString: String = s"$name - $numNodes - $numMutations"
  }

  val typechecker = new Typechecker {}

  private def toCSVRuntime(vals: Seq[(Int, IndexedSeq[(Long, Long)])]): CSV = {
    val header = vals.map(_._1).toIndexedSeq
    // we assume that each list has same number of elements
    val rowLength = vals.head._2.size
    val rows = for (i <- 0 until rowLength) yield vals.map(_._2(i)._1).toIndexedSeq
    header +: rows
  }

  private def toCSVMem(vals: Seq[(Int, IndexedSeq[(Long, Long)])]): CSV = {
    val header = vals.map(_._1).toIndexedSeq
    // we assume that each list has same number of elements
    val rowLength = vals.head._2.size
    val rows = for (i <- 0 until rowLength) yield vals.map(_._2(i)._2).toIndexedSeq
    header +: rows
  }

  private def measureDatalog(c: Config, edb: Seq[Relation], args: Seq[Term], mutationTransformation: Transformation, prog: String = progFolder + "MutationCounter.oinca",  mainClass: String = "Prog", mainMethod: String = "main"): IndexedSeq[(Long, Long)] = {
    val code = FileUtil.readFile(prog)

    val transformations = ObjectOptions.defaultTransformations(mutationTransformation)
    val options = ObjectOptions(transformations = transformations)
    val module = Compiler.compileObject(code, options)

    val name = mutationTransformation.getClass.getSimpleName

    //println(MetricUtils.printStatistics(module.optimized))
    //System.exit(1)

    for (i <- 0 until c.warmup) yield {
      println(s"Warmup ${name} ${c.toString}: ${i + 1}")
      val datalog = new ObjectOrientedDatalog(module)
      datalog.measure(mainClass, mainMethod, edb, args:_*)

      EnginePool.disposeAllEngines()
      MemoryUtil.collectGarbage()
    }
    for (i <- 0 until c.runs) yield {
      println(s"Run ${name} ${c.toString}: ${i + 1}")

      val datalog = new ObjectOrientedDatalog(module)

      /*datalog.update(EDBChange.insertions(edb))
      val run = datalog.run("ProgEntry", "main", args:_*)
      datalog.readAll.foreach { rel =>
        println()
        println(rel.asTable)
      }
      println(run)
      System.exit(1)*/

      MemoryUtil.collectGarbage()
      val memoryBefore = MemoryUtil.usedMemoryInBytes()

      val dt = datalog.measure(mainClass, mainMethod, edb, args:_*)

      MemoryUtil.collectGarbage()
      val memoryAfter = MemoryUtil.usedMemoryInBytes()
      val memoryDelta = memoryAfter - memoryBefore

      println(s"dt: ${dt.toDouble/1000000d} db: ${memoryDelta}")


      EnginePool.disposeAllEngines()
      MemoryUtil.collectGarbage()

      (dt, memoryDelta)
    }
  }

  def runMeasurements(configs: Seq[MutationConfig], subdir: String): Unit = {
    // Structural - EfficientMap
    /*val datalogMeasurementsEfficientStructural = for (c <- configs) yield {
      c.numNodes -> measureDatalog(c, Seq(), Seq(meta.Lit.Int(c.numNodes), meta.Lit.Int(c.numMutations)), EclipseStructuralMutationTransformation)
    }
    FileUtil.writeFile(s"$subdir/EfficientStructuralCounter_Datalog_time.csv", csvToString(toCSVRuntime(datalogMeasurementsEfficientStructural)))
    FileUtil.writeFile(s"$subdir/EfficientStructuralCounter_Datalog_mem.csv", csvToString(toCSVMem(datalogMeasurementsEfficientStructural)))*/

    // Numeric
    val datalogMeasurementsNumeric = for (c <- configs) yield {
      c.numNodes -> measureDatalog(c, Seq(), Seq(meta.Lit.Int(c.numNodes), meta.Lit.Int(c.numMutations)), NumericMutationTransformation)
    }
    FileUtil.writeFile(s"$subdir/NumericCounter_Datalog_time.csv", csvToString(toCSVRuntime(datalogMeasurementsNumeric)))
    FileUtil.writeFile(s"$subdir/NumericCounter_Datalog_mem.csv", csvToString(toCSVMem(datalogMeasurementsNumeric)))

    // Structural - Map
    val datalogMeasurementsStructural = for (c <- configs) yield {
      c.numNodes -> measureDatalog(c, Seq(), Seq(meta.Lit.Int(c.numNodes), meta.Lit.Int(c.numMutations)), StructuralMutationTransformation)
    }
    FileUtil.writeFile(s"$subdir/StructuralCounter_Datalog_time.csv", csvToString(toCSVRuntime(datalogMeasurementsStructural)))
    FileUtil.writeFile(s"$subdir/StructuralCounter_Datalog_mem.csv", csvToString(toCSVMem(datalogMeasurementsStructural)))
  }

  def measureIncreaseNumberOfObjects(): Unit = {
    val numMutations: Int = 10
    val configs = for (i <- 100 until 1010 by 100) yield {
      MutationConfig(warmups, runs, s"Mut", i, numMutations)
    }
    runMeasurements(configs, s"$resultPath/counter/ScaleObjects")
  }

  def measureIncreaseNumberOfMutations(): Unit = {
    val numObjects: Int = 1
    val configs = for (i <- 100 until 1010 by 100) yield {
      MutationConfig(warmups, runs, s"Mut", numObjects, i)
    }
    runMeasurements(configs, s"$resultPath/counter/ScaleMutations")
  }

  def run(): Unit = {
    measureIncreaseNumberOfMutations()
    measureIncreaseNumberOfObjects()
  }
}
