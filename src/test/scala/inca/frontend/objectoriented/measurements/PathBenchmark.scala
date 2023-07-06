package inca.frontend.objectoriented.measurements

import inca.frontend.objectoriented.parser.Parser
import inca.frontend.objectoriented.transformations.{AddMissingDefinitions, InsertBuiltInMonotones}
import inca.frontend.objectoriented.typechecker.Typechecker
import inca.compiler.Compiler
import inca.frontend.objectoriented.compiler.ObjectOptions
import inca.frontend.objectoriented.datalog.ObjectOrientedDatalog
import inca.frontend.objectoriented.interpreter.{Interpreter, ScalaValue}
import inca.runtime.EnginePool
import inca.util.FileUtil
import inca.util.measurement.CSVUtil.{CSV, csvToString}

import scala.meta.XtensionQuasiquoteTerm

object PathBenchmark {
  // TODO what graph
  def options: ObjectOptions = ObjectOptions()

  val progPath: String = "objectoriented/measurements/Path.oinca"
  val typechecker = new Typechecker {}

  val resultPath: String = "benchmark/objectoriented"
  case class Config(warmup: Int, runs: Int, name: String, endNode: Int)

  // 1 -> .. 10 -> endNode  endNode -> 10
  val configs = for (i <- 10 until 10000 by 1000) yield {
    Config(0, 5, s"PATH_${i}", i)
  }

  private def toCSV(vals: Seq[(Int, IndexedSeq[Long])]): CSV = {
    val header = vals.map(_._1).toIndexedSeq
    // we assume that each list has same number of elements
    val rowLength = vals.head._2.size
    val rows = for (i <- 0 until rowLength) yield vals.map(_._2(i)).toIndexedSeq
    header +: rows
  }

  private def measureDatalog(c: Config): IndexedSeq[Long] = {
    val code = FileUtil.readFile(progPath)
    val module = Compiler.compileObject(code, options)

    for (i <- 0 until c.warmup) yield {
      println(s"Warmup Datalog_${c.endNode}: ${i + 1}")
      val datalog = new ObjectOrientedDatalog(module)
      datalog.run("Graph", "main", meta.Lit.Int(c.endNode))
    }
    for (i <- 0 until c.runs) yield {
      println(s"Run Datalog_${c.endNode}: ${i + 1}")
      val datalog = new ObjectOrientedDatalog(module)

      val start = System.nanoTime()
      datalog.run("Graph", "main", meta.Lit.Int(c.endNode))
      val diff = System.nanoTime() - start
      println("Benchmark diff: " + diff.toDouble/1000000d)

      EnginePool.disposeAllEngines()
      System.gc()

      diff
    }
  }

  private def measureInterpreter(c: Config): IndexedSeq[Long] = {
    val code = FileUtil.readFile(progPath)
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
      println(s"Warmup Interpreter_${c.endNode}: ${i + 1}")
      new Interpreter(mod).run(main, Seq(ScalaValue(c.endNode)))
    }
    for (i <- 0 until c.runs) yield {
      println(s"Run Interpreter_${c.endNode}: ${i + 1}")
      val interp = new Interpreter(mod)
      val input = Seq(ScalaValue(c.endNode))

      val start = System.nanoTime()
      interp.run(main, input)
      val diff = System.nanoTime() - start

      EnginePool.disposeAllEngines()
      System.gc()

      diff
    }
  }

  def main(args: Array[String]): Unit = {
    val datalogMeasurements = for (c <- configs) yield {
      c.endNode -> measureDatalog(c)
    }
    FileUtil.writeFile(s"$resultPath/Path_Datalog.csv", csvToString(toCSV(datalogMeasurements)))

    val interpreterMeasurements = for (c <- configs) yield {
      c.endNode -> measureInterpreter(c)
    }
    FileUtil.writeFile(s"$resultPath/Path_Interpreter.csv", csvToString(toCSV(interpreterMeasurements)))
  }
}
