package inca.frontend.objectoriented.measurements

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
  case class MutationConfig(warmup: Int, runs: Int, name: String, numNodes: Int, numMutations: Int) extends Config

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

  private def measureDatalog(c: MutationConfig, prog: String, edb: Seq[Relation], args: Seq[Term], useStructural: Boolean): IndexedSeq[(Long, Long)] = {
    val code = FileUtil.readFile(prog)

    val transformations = ObjectOptions.defaultTransformations(useStructural)
    val options = ObjectOptions(transformations = transformations)
    val module = Compiler.compileObject(code, options)

    val name = if (useStructural) "Structural" else "Numeric"

    //println(MetricUtils.printStatistics(module.optimized))
    //System.exit(1)

    for (i <- 0 until c.warmup) yield {
      println(s"Warmup ${name} ${c.name} - ${c.numNodes}: ${i + 1}")
      val datalog = new ObjectOrientedDatalog(module)
      datalog.measure("Prog", "main", edb, args:_*)

      EnginePool.disposeAllEngines()
      MemoryUtil.collectGarbage()
    }
    for (i <- 0 until c.runs) yield {
      println(s"Run ${name} ${c.name} - ${c.numNodes}: ${i + 1}")

      val datalog = new ObjectOrientedDatalog(module)

      /*datalog.update(EDBChange.insertions(edb))
      val run = datalog.run("ProgEntry", "main", args:_*)
      datalog.readAll.foreach { rel =>
        println()
        println(rel.asTable)
      }
      println(run)
      System.exit(1)*/

      // TODO: Measure Memory footprint

      MemoryUtil.collectGarbage()
      val memoryBefore = MemoryUtil.usedMemoryInBytes()

      val dt = datalog.measure("Prog", "main", edb, args:_*)

      MemoryUtil.collectGarbage()
      val memoryAfter = MemoryUtil.usedMemoryInBytes()
      val memoryDelta = memoryAfter - memoryBefore

      println(s"dt: ${dt.toDouble/1000000d} db: ${memoryDelta}")


      EnginePool.disposeAllEngines()
      MemoryUtil.collectGarbage()

      (dt, memoryDelta)
    }
  }

  def run(): Unit = {
    val numMutations: Int = 10
    val configs = for (i <- 100 until 1010 by 100) yield {
      MutationConfig(warmups, runs, s"Mut", i, numMutations)
    }

    val prog = progFolder + s"MutationCounter.oinca"

    // Numeric
    val datalogMeasurementsNumeric = for (c <- configs) yield {
      c.numNodes -> measureDatalog(c, prog, Seq(), Seq(meta.Lit.Int(c.numNodes), meta.Lit.Int(c.numMutations)), false)
    }
    FileUtil.writeFile(s"$resultPath/mutation/NumericCounter_Datalog_time.csv", csvToString(toCSVRuntime(datalogMeasurementsNumeric)))
    FileUtil.writeFile(s"$resultPath/mutation/NumericCounter_Datalog_mem.csv", csvToString(toCSVMem(datalogMeasurementsNumeric)))

    // Structural
    val datalogMeasurementsStructural = for (c <- configs) yield {
      c.numNodes -> measureDatalog(c, prog, Seq(), Seq(meta.Lit.Int(c.numNodes), meta.Lit.Int(c.numMutations)), true)
    }
    FileUtil.writeFile(s"$resultPath/mutation/StructuralCounter_Datalog_time.csv", csvToString(toCSVRuntime(datalogMeasurementsStructural)))
    FileUtil.writeFile(s"$resultPath/mutation/StructuralCounter_Datalog_mem.csv", csvToString(toCSVMem(datalogMeasurementsStructural)))
  }
}
