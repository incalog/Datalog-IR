package inca.frontend.objectoriented.measurements

import inca.frontend.objectoriented.parser.Parser
import inca.frontend.objectoriented.transformations.{AddMissingDefinitions, InsertBuiltInMonotones}
import inca.frontend.objectoriented.typechecker.Typechecker
import inca.compiler.Compiler
import inca.frontend.objectoriented.compiler.ObjectOptions
import inca.frontend.objectoriented.datalog.ObjectOrientedDatalog
import inca.frontend.objectoriented.interpreter.{Interpreter, ScalaValue, Value}
import inca.runtime.EnginePool
import inca.util.FileUtil
import inca.util.measurement.CSVUtil.{CSV, csvToString}
import inca.util.measurement.MemoryUtil

import scala.meta.{Term, XtensionQuasiquoteTerm}

object PathBenchmark {
  // TODO what graph
  val recursive = "right"
  val warmups = 5
  val runs = 3

  val progFolder: String = s"objectoriented/measurements/"
  val resultPath: String = "benchmark/objectoriented"

  trait Config {
    val warmup: Int
    val runs: Int
    val name: String
  }
  case class PathConfig(warmup: Int, runs: Int, name: String, endNode: Int) extends Config
  case class PathAllocationConfig(warmup: Int, runs: Int, name: String, heapSize: Int) extends Config


  def options: ObjectOptions = ObjectOptions()
  val typechecker = new Typechecker {}

  private def toCSV(vals: Seq[(Int, IndexedSeq[Long])]): CSV = {
    val header = vals.map(_._1).toIndexedSeq
    // we assume that each list has same number of elements
    val rowLength = vals.head._2.size
    val rows = for (i <- 0 until rowLength) yield vals.map(_._2(i)).toIndexedSeq
    header +: rows
  }

  private def measureDatalog(c: Config, prog: String, args: Seq[Term]): IndexedSeq[Long] = {
    val code = FileUtil.readFile(prog)
    val module = Compiler.compileObject(code, options)

    for (i <- 0 until c.warmup) yield {
      println(s"Warmup Datalog ${c.name}: ${i + 1}")
      val datalog = new ObjectOrientedDatalog(module)
      datalog.run("Graph", "main", args:_*)
    }
    for (i <- 0 until c.runs) yield {
      println(s"Run Datalog ${c.name}: ${i + 1}")
      val datalog = new ObjectOrientedDatalog(module)

      val start = System.nanoTime()
      datalog.measure("Graph", "main", args:_*)
      val diff = System.nanoTime() - start
      println("diff: " + diff.toDouble/1000000d)

      EnginePool.disposeAllEngines()
      //System.gc()
      MemoryUtil.collectGarbage()

      diff
    }
  }

  private def measureInterpreter(c: Config, prog: String, args: Seq[Value]): IndexedSeq[Long] = {
    val code = FileUtil.readFile(prog)
    var mod = Parser.parse(code)
    mod = InsertBuiltInMonotones.transformModule(mod)
    mod = AddMissingDefinitions.transformModule(mod)
    typechecker.typecheck(mod)

    val mainClass = "Graph"
    val mainMethod = "main"
    val mainClasses = mod.classes.filter(_.name.raw == mainClass)
    val mainMethods = mainClasses.flatMap(c => c.methods.filter(_.name.raw == mainMethod))
    if (mainMethods.size < 1) {
      throw new IllegalArgumentException(s"No main method with name $mainMethod for class $mainClass found!")
    } else if (mainMethods.size > 1) {
      throw new IllegalArgumentException(s"Ambiguous method with name $mainMethod for class $mainClass found!")
    }

    val main = mainMethods.head
    if (!main.isMain) {
      throw new IllegalArgumentException(s"Method $mainMethod for class $mainClass is not a main method!")
    }

    for (i <- 0 until c.warmup) yield {
      println(s"Warmup Interpreter ${c.name}: ${i + 1}")
      new Interpreter(mod).run(main, args) // Seq(ScalaValue(c.endNode))
    }
    for (i <- 0 until c.runs) yield {
      println(s"Run Interpreter ${c.name}: ${i + 1}")
      val interp = new Interpreter(mod)

      val start = System.nanoTime()
      interp.run(main, args)
      val diff = System.nanoTime() - start
      println("diff: " + diff.toDouble/1000000d)

      EnginePool.disposeAllEngines()
      //System.gc()
      MemoryUtil.collectGarbage()

      diff
    }
  }

  def runPath() = {
    // 1 -> .. 10 -> endNode  endNode -> 10
    val configs = for (i <- 10 until 140 by 20) yield {
      PathConfig(warmups, runs, s"PATH_${i}", i)
    }

    val prog = progFolder + s"Path_$recursive.oinca"
    val datalogMeasurements = for (c <- configs) yield {
      c.endNode -> measureDatalog(c, prog, Seq(meta.Lit.Int(c.endNode)))
    }
    FileUtil.writeFile(s"$resultPath/Path_Datalog_${recursive}_recursive.csv", csvToString(toCSV(datalogMeasurements)))

    val interpreterMeasurements = for (c <- configs) yield {
      c.endNode -> measureInterpreter(c, prog, Seq(ScalaValue(c.endNode)))
    }
    FileUtil.writeFile(s"$resultPath/Path_Interpreter_${recursive}_recursive.csv", csvToString(toCSV(interpreterMeasurements)))
  }

  def runPathWithAllocation() = {
    val configs = for (i <- 10 until 50000 by 5000) yield {
      PathAllocationConfig(warmups, runs, s"PATH_ALLOC_${i}", i)
    }
    val prog = progFolder + s"Path_${recursive}_dummy.oinca"

    val datalogMeasurements = for (c <- configs) yield {
      c.heapSize -> measureDatalog(c, prog, Seq(q"50", meta.Lit.Int(c.heapSize)))
    }
    FileUtil.writeFile(s"$resultPath/Path_Datalog_${recursive}_recursive_dummy.csv", csvToString(toCSV(datalogMeasurements)))

    val interpreterMeasurements = for (c <- configs) yield {
      c.heapSize -> measureInterpreter(c, prog, Seq(ScalaValue(50), ScalaValue(c.heapSize)))
    }
    FileUtil.writeFile(s"$resultPath/Path_Interpreter_${recursive}_recursive_dummy.csv", csvToString(toCSV(interpreterMeasurements)))
  }


  def runPathWithCycles() = {
    val configs = for (i <- 10 until 21 by 2) yield {
      PathConfig(warmups, runs, s"PATH_CYCLES_${i}", i)
    }
    val prog = progFolder + s"Path_${recursive}_cycles.oinca"

    val interpreterMeasurements = for (c <- configs) yield {
      c.endNode -> measureInterpreter(c, prog, Seq(ScalaValue(c.endNode)))
    }
    FileUtil.writeFile(s"$resultPath/Path_Interpreter_${recursive}_recursive_cycles.csv", csvToString(toCSV(interpreterMeasurements)))

    val datalogMeasurements = for (c <- configs) yield {
      c.endNode -> measureDatalog(c, prog, Seq(meta.Lit.Int(c.endNode)))
    }
    FileUtil.writeFile(s"$resultPath/Path_Datalog_${recursive}_recursive_cycles.csv", csvToString(toCSV(datalogMeasurements)))
  }

  def main(args: Array[String]): Unit = {
    runPathWithCycles()
  }
}
