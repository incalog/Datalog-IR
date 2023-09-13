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

case class WhileBenchmark(val warmups: Int, val runs: Int) {
  val progFolder: String = s"objectoriented/measurements/"
  val resultPath: String = "benchmark/objectoriented"

  trait Config {
    val warmup: Int
    val runs: Int
    val name: String
  }
  case class WhileConfig(warmup: Int, runs: Int, name: String, endNode: Int, step: Int) extends Config


  def options: ObjectOptions = ObjectOptions()
  val typechecker = new Typechecker {}

  private def toCSV(vals: Seq[(Int, IndexedSeq[Long])]): CSV = {
    val header = vals.map(_._1).toIndexedSeq
    // we assume that each list has same number of elements
    val rowLength = vals.head._2.size
    val rows = for (i <- 0 until rowLength) yield vals.map(_._2(i)).toIndexedSeq
    header +: rows
  }

  private def measureDatalog(c: Config, prog: String, edb: Seq[Relation], args: Seq[Term]): IndexedSeq[Long] = {
    val code = FileUtil.readFile(prog)
    val module = Compiler.compileObject(code, options)

    for (i <- 0 until c.warmup) yield {
      println(s"Warmup Datalog ${c.name}: ${i + 1}")
      val datalog = new ObjectOrientedDatalog(module)
      datalog.measure("ConstantPropagation", "main", edb, args:_*)

      EnginePool.disposeAllEngines()
      MemoryUtil.collectGarbage()
    }
    for (i <- 0 until c.runs) yield {
      println(s"Run Datalog ${c.name}: ${i + 1}")

      val datalog = new ObjectOrientedDatalog(module)

      /*datalog.update(EDBChange.insertions(edb))
      val run = datalog.run("Examples", "main", args:_*)
      datalog.readAll.foreach { rel =>
        println()
        println(rel.asTable)
      }
      println(run)
      System.exit(1)*/

      val start = System.nanoTime()
      datalog.measure("Examples", "main", edb, args:_*)
      val diff = System.nanoTime() - start
      println("diff: " + diff.toDouble/1000000d)

      EnginePool.disposeAllEngines()
      MemoryUtil.collectGarbage()

      diff
    }
  }

  private def measureInterpreter(c: Config, prog: String, edb: Map[String, Value], args: Seq[Value]): IndexedSeq[Long] = {
    val code = FileUtil.readFile(prog)
    var mod = Parser.parse(code)
    mod = InsertBuiltInMonotones.transformModule(mod)
    mod = AddMissingDefinitions.transformModule(mod)
    typechecker.typecheck(mod)
    if (typechecker.getErrors.nonEmpty)
      throw new Exception(typechecker.getErrors.mkString("\n"))

    val mainClass = "ConstantPropagation"
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
      new Interpreter(mod, edb).run(main, args)

      MemoryUtil.collectGarbage()
    }
    for (i <- 0 until c.runs) yield {
      println(s"Run Interpreter ${c.name}: ${i + 1}")
      val interp = new Interpreter(mod, edb)

      val start = System.nanoTime()
      interp.run(main, args)
      val diff = System.nanoTime() - start
      println("diff: " + diff.toDouble/1000000d)
      /*println(res.asSet.size)
      println(res.asSet)
      System.exit(1)*/

      MemoryUtil.collectGarbage()

      diff
    }
  }

  def run() = {
    val configs = for (i <- 100 until 200 by 50) yield {
      WhileConfig(warmups, runs, s"While", i, 10)
    }

    val prog = progFolder + s"WhileLang.oinca"

    // OODL - Interp
    val interpreterMeasurements = for (c <- configs) yield {
      c.endNode -> measureInterpreter(c, prog, Map(), Seq(ScalaValue(c.endNode), ScalaValue(c.step)))
    }
    FileUtil.writeFile(s"$resultPath/while/While_Interpreter.csv", csvToString(toCSV(interpreterMeasurements)))

    // OODL
    val datalogMeasurements = for (c <- configs) yield {
      c.endNode -> measureDatalog(c, prog, Seq(), Seq(meta.Lit.Int(c.endNode), meta.Lit.Int(c.step)))
    }
    FileUtil.writeFile(s"$resultPath/while/While_Datalog.csv", csvToString(toCSV(datalogMeasurements)))
  }
}
