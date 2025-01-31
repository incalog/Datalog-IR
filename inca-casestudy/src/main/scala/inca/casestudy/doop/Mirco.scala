package inca.casestudy.doop

import inca.ir.{CompiledUnit, string2name}
import inca.ir.execution.{IRExecutor, ThreadCount, UnitRelation}
import inca.ir.execution.ThreadCount.{Auto, Fixed}
import inca.ir.extension.{block, bool, disjunction, module, not}
import inca.ir.optimize.AliasElimination
import inca.ir.valueNumbering.ValueNumbering
import inca.souffle.frontend.compile.CompiledSouffleProgram
import inca.util.compileroptions.CompilerOptions
import inca.viatra.backend
import inca.viatra.backend.Executor

import scala.io.Source

object Mirco:

  private def runMicroDL(createEngine: (compiled: CompiledUnit) => IRExecutor#Engine, file: String = "micro.dl"): Unit =
    val baseDir = "doop/"
    val source = Source.fromResource(baseDir + file)
    val options = CompilerOptions.default
    //options.irLogging.logLowerings = true
    val compiled = CompiledSouffleProgram.fromSource("micro", source, options)
    compiled.setPipeline(List(
      () => new bool.Lowering {},
      () => new block.Lowering {},
      () => new disjunction.Lowering {},
      () => new not.Lowering {},
//      () => new AliasElimination {},
      () => new module.Lowering {},
      () => new ValueNumbering()
    ))

    println("Load edb from files...")
    val edbFacts = compiled.loadEdbInputs(baseDir + "minijavac")
    val outputRels = compiled.outputRelations

    println("Populate edb...")
    val engine = createEngine(compiled.mainUnit)
    edbFacts.foreach(engine.insert)

    println("Execute...")
    val execTime = outputRels.map { rel =>
      val start = System.currentTimeMillis()
      val res = engine.read(rel)
      val end = System.currentTimeMillis()
      println(res.name -> res.size)
      end - start
    }.sum

    /*val start = System.currentTimeMillis()
    engine.read(UnitRelation("VarPointsTo"))
    val end = System.currentTimeMillis()
    val execTime = end - start*/

    println(execTime / 1000.0)

  @main
  def runMicroDlSouffleOriginal(): Unit = {
    runMicroDL(
      compiled => inca.souffle.backend.Executor(Fixed(1)).instantiate(compiled),
      "micro-original.dl"
    )
  }

  @main
  def runMicroDlSouffle(): Unit = {
    runMicroDL(compiled => inca.souffle.backend.Executor(Fixed(1)).instantiate(compiled))
  }

  @main
  def runMicroDlSouffleParallel(): Unit = {
    runMicroDL(compiled => inca.souffle.backend.Executor(Auto).instantiate(compiled))
  }

  @main
  def runMicroDlViatra(): Unit = {
    backend.Executor.initializeLogging()
    //inca.viatra.Executor.enableDebugLogging()
    runMicroDL(compiled => Executor().instantiate(compiled))
  }

  @main
  def runMicroDlAscent(): Unit = {
    runMicroDL(compiled => inca.ascent.backend.Executor(Fixed(1)).instantiate(compiled))
  }

  @main
  def runMicroDlAscentParallel(): Unit = {
    runMicroDL(compiled => inca.ascent.backend.Executor(Auto).instantiate(compiled))
  }


