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

case class PathBenchmark(val warmups: Int, val runs: Int) {
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

      /*val run = datalog.run("Graph", "main", args:_*)
      println(run.size)
      val s = run.toSet
      println(s.size)
      println(s)*/
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
      //println("diff: " + diff.toDouble/1000000d)
      //println(res.asSet.size)
      //System.exit(1)

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

  def runPathWithEDB(recursive: String) = {
    // 1 -> .. 10 -> endNode  endNode -> 10
    val configs = for (i <- 10 until 140 by 20) yield {
      PathConfig(warmups, runs, s"PATH_${i}", i)
    }

    val prog = progFolder + s"Path_${recursive}_edb.oinca"

    // IR with graph in edb
    val irMeasurements = for (c <- configs) yield {
      // Note: Make sure this code produces the same graph as the program
      val edb = EDBChange.insertions(Seq(Relation2("edge", Seq("x", "y"), PathIRModule.input(c.endNode))))
      c.endNode -> measureDatalogIR(c, PathIRModule.module(recursive), "path", edb)
    }
    FileUtil.writeFile(s"$resultPath/edb/Path_IR_${recursive}_recursive.csv", csvToString(toCSV(irMeasurements)))

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
    val datalogMeasurements = for (c <- configs) yield {
      val edb = Relation.from("edbEdges", Seq("x", "y"), PathIRModule.input(c.endNode))
      c.endNode -> measureDatalog(c, prog, Seq(edb), Seq())
    }
    FileUtil.writeFile(s"$resultPath/edb/Path_Datalog_${recursive}_recursive.csv", csvToString(toCSV(datalogMeasurements)))

    // OODL - Interp
    val interpreterMeasurements = for (c <- configs) yield {
      val edges = PathIRModule.input(c.endNode)
      val edb = Map("edbEdges" -> SetValue(edges.map(is => TupleValue(is.map(ScalaValue))).toSet))
      c.endNode -> measureInterpreter(c, prog, edb, Seq())
    }
    FileUtil.writeFile(s"$resultPath/edb/Path_Interpreter_${recursive}_recursive.csv", csvToString(toCSV(interpreterMeasurements)))
  }

  def runPathAllocationWithEDB(recursive: String) = {
    val configs = for (i <- 10 until 50000 by 5000) yield {
      PathAllocationConfig(warmups, runs, s"PATH_ALLOC_${i}", i)
    }
    val prog = progFolder + s"Path_${recursive}_dummy_edb.oinca"
    val edges = PathIRModule.input(50)

    // OODL
    val datalogMeasurements = for (c <- configs) yield {
      val edb = Relation.from("edbEdges", Seq("x", "y"), edges)
      c.heapSize -> measureDatalog(c, prog, Seq(edb), Seq( meta.Lit.Int(c.heapSize)))
    }
    FileUtil.writeFile(s"$resultPath/edb/Path_Datalog_${recursive}_recursive_alloc.csv", csvToString(toCSV(datalogMeasurements)))

    // OODL - Interp
    val interpreterMeasurements = for (c <- configs) yield {
      val edb = Map("edbEdges" -> SetValue(edges.map(is => TupleValue(is.map(ScalaValue))).toSet))
      c.heapSize -> measureInterpreter(c, prog, edb, Seq(ScalaValue(c.heapSize)))
    }
    FileUtil.writeFile(s"$resultPath/edb/Path_Interpreter_${recursive}_recursive_alloc.csv", csvToString(toCSV(interpreterMeasurements)))
  }

  def runPathCyclesWithEDB(recursive: String) = {
    // number of nodes should be: n * k + (sum i=(k+1) to n (n-i))
    // where k is the last node that is fully connected

    // Good for right config
    val configs =
      if (recursive == "right") {
        for (i <- Seq(8, 4, 2, 1)) yield
          PathCycleConfig(warmups, runs, s"PATH_CYCLES_${i}", 8, i)
      } else {
        for (i <- Seq(8, 4, 2, 1)) yield
          PathCycleConfig(warmups, runs, s"PATH_CYCLES_${i}", 40, i)
      }
    val prog = progFolder + s"Path_${recursive}_edb.oinca"

    // IR
    val irMeasurements = for (c <- configs) yield {
      // Note: Make sure this code produces the same graph as the program
      val edb = EDBChange.insertions(Seq(Relation2("edge", Seq("x", "y"), PathIRModule.inputWithCycle(c.cycleStep, c.endNode))))
      c.endNode -> measureDatalogIR(c, PathIRModule.module(recursive), "path", edb)
    }
    FileUtil.writeFile(s"$resultPath/edb/Path_IR_${recursive}_recursive_cycles.csv", csvToString(toCSV(irMeasurements)))

    // OODL
    val datalogMeasurements = for (c <- configs) yield {
      val edb = Relation.from("edbEdges", Seq("x", "y"), PathIRModule.inputWithCycle(c.cycleStep, c.endNode))
      c.cycleStep -> measureDatalog(c, prog, Seq(edb), Seq())
    }
    FileUtil.writeFile(s"$resultPath/edb/Path_Datalog_${recursive}_recursive_cycles.csv", csvToString(toCSV(datalogMeasurements)))

    // OODL - Interp
    val interpreterMeasurements = for (c <- configs) yield {
      val edges = PathIRModule.inputWithCycle(c.cycleStep, c.endNode)
      val edb = Map("edbEdges" -> SetValue(edges.map(is => TupleValue(is.map(ScalaValue))).toSet))
      c.cycleStep -> measureInterpreter(c, prog, edb, Seq())
    }
    FileUtil.writeFile(s"$resultPath/edb/Path_Interpreter_${recursive}_recursive_cycles.csv", csvToString(toCSV(interpreterMeasurements)))
  }

  def runPathWithoutEDB(recursive: String) = {
    // 1 -> .. 10 -> endNode  endNode -> 10
    val configs = for (i <- 10 until 140 by 20) yield {
      PathConfig(warmups, runs, s"PATH_${i}", i)
    }

    val prog = progFolder + s"Path_$recursive.oinca"

    // IR with computed graph
    val irMeasurements = for (c <- configs) yield {
      val edb = EDBChange.insertions(Seq(Relation1("ext_input$main$bff", Seq("e"), Seq(Seq(c.endNode)))))
      c.endNode -> measureDatalogIR(c, PathIRModule.moduleWithInputComputation(recursive), "main", edb)
    }
    FileUtil.writeFile(s"$resultPath/no_edb/Path_IR_${recursive}_recursive.csv", csvToString(toCSV(irMeasurements)))

    // OODL
    val datalogMeasurements = for (c <- configs) yield {
      c.endNode -> measureDatalog(c, prog, Seq(), Seq(meta.Lit.Int(c.endNode)))
    }
    FileUtil.writeFile(s"$resultPath/no_edb/Path_Datalog_${recursive}_recursive.csv", csvToString(toCSV(datalogMeasurements)))

    // OODL - Interp
    val interpreterMeasurements = for (c <- configs) yield {
      c.endNode -> measureInterpreter(c, prog, Map(), Seq(ScalaValue(c.endNode)))
    }
    FileUtil.writeFile(s"$resultPath/Path_Interpreter_${recursive}_recursive.csv", csvToString(toCSV(interpreterMeasurements)))
  }

  def runPathNodesWithoutEDB(recursive: String) = {
    // 1 -> .. 10 -> endNode  endNode -> 10
    val configs = for (i <- 50 until 60 by 20) yield {
      PathConfig(warmups, runs, s"PATH_${i}", i)
    }

    val prog = progFolder + s"Path_${recursive}_node.oinca"

    // OODL
    val datalogMeasurements = for (c <- configs) yield {
      c.endNode -> measureDatalog(c, prog, Seq(), Seq(meta.Lit.Int(c.endNode)))
    }
    FileUtil.writeFile(s"$resultPath/no_edb/Path_Datalog_${recursive}_recursive_nodes.csv", csvToString(toCSV(datalogMeasurements)))

    // OODL - Interp
    val interpreterMeasurements = for (c <- configs) yield {
      c.endNode -> measureInterpreter(c, prog, Map(), Seq(ScalaValue(c.endNode)))
    }
    FileUtil.writeFile(s"$resultPath/no_edb/Path_Interpreter_${recursive}_recursive_nodes.csv", csvToString(toCSV(interpreterMeasurements)))
  }

  def runSec3(recursive: String) = {
    // 1 -> .. 10 -> endNode  endNode -> 10
    val configs = for (i <- 10 until 160 by 20) yield {
      PathCycleConfig(warmups, runs, s"PATH_${i}", i, 10)
    }

    // IR with computed graph
    val irMeasurements = for (c <- configs) yield {
      val edb = EDBChange.insertions(Seq(Relation2("ext_input$main$bbff", Seq("e", "s"), Seq(Seq(c.endNode, c.cycleStep)))))
      c.endNode -> measureDatalogIR(c, PathIRModule.sec3Module(recursive), "main", edb)
    }
    FileUtil.writeFile(s"$resultPath/sec3/Path_IR_${recursive}_recursive.csv", csvToString(toCSV(irMeasurements)))

    // OODL - Interp
    val prog1 = progFolder + s"sec3/Path_$recursive.oinca"
    println(s"Run prog $prog1")
    val interpreterMeasurements1 = for (c <- configs) yield {
      c.endNode -> measureInterpreter(c, prog1, Map(), Seq(ScalaValue(c.endNode), ScalaValue(c.cycleStep)))
    }
    FileUtil.writeFile(s"$resultPath/sec3/Path_Interpreter_${recursive}_recursive.csv", csvToString(toCSV(interpreterMeasurements1)))

    // OODL - Interp
    val prog2 = progFolder + s"sec3/Path_${recursive}_node.oinca"
    println(s"Run prog $prog2")
    val interpreterMeasurements2 = for (c <- configs) yield {
      c.endNode -> measureInterpreter(c, prog2, Map(), Seq(ScalaValue(c.endNode), ScalaValue(c.cycleStep)))
    }
    FileUtil.writeFile(s"$resultPath/sec3/Path_Interpreter_${recursive}_recursive_node.csv", csvToString(toCSV(interpreterMeasurements2)))
  }

  def run(recursive: String): Unit = {
    //runSec3(recursive)
    //runPathWithEDB(recursive)
    //runPathAllocationWithEDB(recursive)
    runPathCyclesWithEDB(recursive)
    /*runPathWithoutEDB(recursive)
    runPathNodesWithoutEDB(recursive)*/
  }
}
