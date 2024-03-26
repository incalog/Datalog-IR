package inca.casestudy.doop

import inca.ir.{CompiledModule, SimpleAliasElimination, string2name}
import inca.ir.execution.{IRExecutor, UnitRelation}
import inca.ir.execution.ThreadCount.{Auto, Fixed}
import inca.ir.extension.{block, bool, disjunction, not}
import inca.souffle.frontend.compile.CompiledSouffleModule

import scala.io.Source

object Mirco:

  private def runMicroDL(createEngine: (compiled: CompiledModule) => IRExecutor#Engine): Unit =
    val baseDir = "doop/"
    val source = Source.fromResource(baseDir + "micro.dl")
    val compiled = CompiledSouffleModule.fromSource("micro", source)
    compiled.setPipeline(List(
      () => new bool.Lowering {},
      () => new block.Lowering {},
      () => new disjunction.Lowering {},
      () => new not.Lowering {},
      () => new SimpleAliasElimination {}
    ))

    println("Load edb from files...")
    val edbFacts = compiled.loadEdbInputs(baseDir + "minijavac")
    val outputRels = compiled.outputRelations

    println("Populate edb...")
    val engine = createEngine(compiled)
    edbFacts.foreach(engine.insert)

    println("Execute...")
    /*val execTime = outputRels.map { rel =>
      val start = System.currentTimeMillis()
      val res = engine.read(rel)
      val end = System.currentTimeMillis()
      println(res.name -> res.size)
      end - start
    }.sum*/

    val start = System.currentTimeMillis()
    engine.read(UnitRelation("VarPointsTo"))
    val end = System.currentTimeMillis()
    val execTime = end - start

    println(execTime / 1000.0)

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
    inca.viatra.Executor.initializeLogging()
    //inca.viatra.Executor.enableDebugLogging()

    runMicroDL(compiled => inca.viatra.Executor().instantiate(compiled))
  }

  @main
  def runMicroDlAscent(): Unit = {
    runMicroDL(compiled => inca.ascent.backend.Executor(Fixed(1)).instantiate(compiled))
  }

  @main
  def runMicroDlAscentParallel(): Unit = {
    runMicroDL(compiled => inca.ascent.backend.Executor(Auto).instantiate(compiled))
  }


