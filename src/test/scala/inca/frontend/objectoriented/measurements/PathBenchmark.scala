package inca.frontend.objectoriented.measurements

import inca.frontend.objectoriented.parser.Parser
import inca.frontend.objectoriented.transformations.{AddMissingDefinitions, InsertBuiltInMonotones}
import inca.frontend.objectoriented.typechecker.Typechecker
import inca.compiler.{CompiledModule, Compiler}
import inca.frontend.ir.{EDBChange, Relation, Relation1, Relation2, Relation3, Datalog => DatalogAPI}
import inca.frontend.objectoriented.compiler.ObjectOptions
import inca.frontend.objectoriented.datalog.ObjectOrientedDatalog
import inca.frontend.objectoriented.interpreter.{Interpreter, ScalaValue, SetValue, TupleValue, Value}
import inca.runtime.EnginePool
import inca.util.FileUtil
import inca.util.measurement.CSVUtil.{CSV, csvToString}
import inca.util.measurement.MemoryUtil

import scala.meta.{Term, XtensionQuasiquoteTerm}

object PathBenchmark {
  // TODO what graph
  val recursive = "right"
  val edbEdges = true
  def edbEdgesSuffix: String = if (edbEdges) "_edb" else ""
  val warmups = 0
  val runs = 1

  val progFolder: String = s"objectoriented/measurements/"
  val resultPath: String = "benchmark/objectoriented"

  trait Config {
    val warmup: Int
    val runs: Int
    val name: String
  }
  case class PathConfig(warmup: Int, runs: Int, name: String, endNode: Int) extends Config
  case class PathAllocationConfig(warmup: Int, runs: Int, name: String, heapSize: Int) extends Config
  case class PathCycleConfig(warmup: Int, runs: Int, name: String, endNode: Int, cycleStep: Int) extends Config


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
      datalog.measure("Graph", "main", edb, args:_*)

      EnginePool.disposeAllEngines()
      MemoryUtil.collectGarbage()
    }
    for (i <- 0 until c.runs) yield {
      println(s"Run Datalog ${c.name}: ${i + 1}")

      val datalog = new ObjectOrientedDatalog(module)

      //datalog.update(EDBChange.insertions(edb))
      //val run = datalog.run("Graph", "main", args:_*)
      //println(run.size)
      //println(run)
      //System.exit(1)

      val start = System.nanoTime()
      datalog.measure("Graph", "main", edb, args:_*)
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

      MemoryUtil.collectGarbage()

      diff
    }
  }

  private def measureDatalogIR(c: Config, module: CompiledModule, name: String, edb: EDBChange): IndexedSeq[Long]  = {
    // TODO: To make the measurement fair, we would actually need to generate the data
    //  inside the program as well

    for (i <- 0 until c.warmup) yield {
      println(s"Warmup Datalog IR ${c.name}: ${i + 1}")
      val datalog: DatalogAPI = new DatalogAPI(module)
      datalog.measure(name, edb)

      EnginePool.disposeAllEngines()
      MemoryUtil.collectGarbage()
    }
    for (i <- 0 until c.runs) yield {
      println(s"Run Datalog IR ${c.name}: ${i + 1}")
      val datalog: DatalogAPI = new DatalogAPI(module)

      //datalog.update(edb)
      //println(datalog.read(Relation2(name, Seq("X", "Y"), Seq())).size)
      //println(datalog.read(Relation2(name, Seq("X", "Y"), Seq())))
      //System.exit(1)

      val start = System.nanoTime()
      datalog.measure(name, edb)
      val diff = System.nanoTime() - start
      println("diff: " + diff.toDouble / 1000000d)

      EnginePool.disposeAllEngines()
      MemoryUtil.collectGarbage()

      diff
    }
  }

  def runPath() = {
    // 1 -> .. 10 -> endNode  endNode -> 10
    val configs = for (i <- 10 until 140 by 20) yield {
      PathConfig(warmups, runs, s"PATH_${i}", i)
    }

    val prog = progFolder + s"Path_$recursive$edbEdgesSuffix.oinca"

    // IR with graph in edb
    /*val irMeasurements = for (c <- configs) yield {
      // Note: Make sure this code produces the same graph as the program
      val edb = EDBChange.insertions(Seq(Relation2("edge", Seq("x", "y"), PathIRModule.input(c.endNode))))
      c.endNode -> measureDatalogIR(c, PathIRModule.module(recursive), "path", edb)
    }
    FileUtil.writeFile(s"$resultPath/Path_IR_${recursive}${edbEdgesSuffix}_recursive.csv", csvToString(toCSV(irMeasurements)))*/

    // IR with computed graph
    /*val irMeasurements = for (c <- configs) yield {
      if (edbEdges) {
        val edges = PathIRModule.input(c.endNode)
        val edb = EDBChange.insertions(Seq(Relation2("edge", Seq("X", "Y"), edges)))
        c.endNode -> measureDatalogIR(c, PathIRModule.module(recursive), "path", edb)
      } else {
        val edb = EDBChange.insertions(Seq(Relation1("ext_input$main$bff", Seq("e"), Seq(Seq(c.endNode)))))
        c.endNode -> measureDatalogIR(c, PathIRModule.module(recursive), "main", edb)
      }
    }
    FileUtil.writeFile(s"$resultPath/Path_IR_${recursive}${edbEdgesSuffix}_recursive.csv", csvToString(toCSV(irMeasurements)))*/

    // OODL
    /*val datalogMeasurements = for (c <- configs) yield {
      val edb = Relation.from("edbEdges", Seq("x", "y"), PathIRModule.input(c.endNode))
      c.endNode -> measureDatalog(c, prog, Seq(edb), Seq(meta.Lit.Int(c.endNode)))
    }
    FileUtil.writeFile(s"$resultPath/Path_Datalog_${recursive}${edbEdgesSuffix}_recursive.csv", csvToString(toCSV(datalogMeasurements)))*/

    // OODL - Interp
    val interpreterMeasurements = for (c <- configs) yield {
      val edges = PathIRModule.input(c.endNode)
      val edb = Map("edbEdges" -> SetValue(edges.map(is => TupleValue(is.map(ScalaValue))).toSet))
      c.endNode -> measureInterpreter(c, prog, edb, Seq(ScalaValue(c.endNode)))
    }
    FileUtil.writeFile(s"$resultPath/Path_Interpreter_${recursive}${edbEdgesSuffix}_recursive.csv", csvToString(toCSV(interpreterMeasurements)))
  }

  /*def runPathWithAllocation() = {
    val configs = for (i <- 10 until 50000 by 5000) yield {
      PathAllocationConfig(warmups, runs, s"PATH_ALLOC_${i}", i)
    }
    val prog = progFolder + s"Path_${recursive}_dummy.oinca"

    // IR
    // There is no concept of objects or allocation in pure Datalog

    // OODL
    val datalogMeasurements = for (c <- configs) yield {
      c.heapSize -> measureDatalog(c, prog, Seq(q"50", meta.Lit.Int(c.heapSize)))
    }
    FileUtil.writeFile(s"$resultPath/Path_Datalog_${recursive}_recursive_dummy.csv", csvToString(toCSV(datalogMeasurements)))

    // OODL - Interp
    val interpreterMeasurements = for (c <- configs) yield {
      c.heapSize -> measureInterpreter(c, prog, Seq(ScalaValue(50), ScalaValue(c.heapSize)))
    }
    FileUtil.writeFile(s"$resultPath/Path_Interpreter_${recursive}_recursive_dummy.csv", csvToString(toCSV(interpreterMeasurements)))
  }

  def runPathWithCycles() = {
    val configs = for (i <- Seq(15, 7, 5, 3, 2, 1)) yield {
      PathCycleConfig(warmups, runs, s"PATH_CYCLES_${i}", 150, i)
    }
    val prog = progFolder + s"Path_${recursive}_cycles.oinca"

    // IR
    val input = Relation2("path", Seq("X", "Y"), Seq())
    val irMeasurements = for (c <- configs) yield {
      // Note: Make sure this code produces the same graph as the program
      val edb = EDBChange.insertions(Seq(Relation2("edge", Seq("x", "y"), PathIRModule.inputWithCycle(c.cycleStep, c.endNode))))
      c.endNode -> measureDatalogIR(c, PathIRModule.module(recursive), "path", edb)
    }
    FileUtil.writeFile(s"$resultPath/Path_IR_${recursive}_recursive_cycles.csv", csvToString(toCSV(irMeasurements)))

    // OODL
    val datalogMeasurements = for (c <- configs) yield {
      c.cycleStep -> measureDatalog(c, prog, Seq(meta.Lit.Int(c.endNode), meta.Lit.Int(c.cycleStep)))
    }
    FileUtil.writeFile(s"$resultPath/Path_Datalog_${recursive}_recursive_cycles.csv", csvToString(toCSV(datalogMeasurements)))

    // OODL - Interp
    val interpreterMeasurements = for (c <- configs) yield {
      c.cycleStep -> measureInterpreter(c, prog, Seq(ScalaValue(c.endNode), ScalaValue(c.cycleStep)))
    }
    FileUtil.writeFile(s"$resultPath/Path_Interpreter_${recursive}_recursive_cycles.csv", csvToString(toCSV(interpreterMeasurements)))
  }

  def runPathWithCyclesWorstCase() = {
    // number of nodes should be: n * k + (sum i=(k+1) to n (n-i))
    // where k is the last node that is fully connected
    val configs = for (i <- 120 until 0 by -17) yield {
      PathCycleConfig(warmups, runs, s"PATH_CYCLES_${i}", 120, i)
    }
    val prog = progFolder + s"Path_${recursive}_cycles_worstcase.oinca"

    // TODO: Support IR

    // OODL
    val datalogMeasurements = for (c <- configs) yield {
      c.cycleStep -> measureDatalog(c, prog, Seq(meta.Lit.Int(c.endNode), meta.Lit.Int(c.cycleStep)))
    }
    FileUtil.writeFile(s"$resultPath/Path_Datalog_${recursive}_recursive_cycles.csv", csvToString(toCSV(datalogMeasurements)))

    // OODL - Interp
    val interpreterMeasurements = for (c <- configs) yield {
      c.cycleStep -> measureInterpreter(c, prog, Seq(ScalaValue(c.endNode), ScalaValue(c.cycleStep)))
    }
    FileUtil.writeFile(s"$resultPath/Path_Interpreter_${recursive}_recursive_cycles_worstcase.csv", csvToString(toCSV(interpreterMeasurements)))
  }*/

  def main(args: Array[String]): Unit = {
    runPath()
    //runPathWithAllocation()
    //runPathWithCycles()
    //runPathWithCyclesWorstCase()
  }
}
