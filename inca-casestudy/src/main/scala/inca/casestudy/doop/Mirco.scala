package inca.casestudy.doop

import inca.ir.{CompiledModule, SimpleAliasElimination, string2name}
import inca.ir.execution.IRExecutor
import inca.ir.extension.{block, bool, disjunction, not}
import inca.souffle.frontend.compile.CompiledSouffleModule

import scala.io.Source

object Mirco:

  private def runMicroDL(createEngine: (compiled: CompiledModule) => IRExecutor#Engine) =
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
    val rels = outputRels.map { rel =>
      val res = engine.read(rel)
      println(res.name -> res.size)
    }

  @main
  def runMicroDlSouffle(): Unit = {
    runMicroDL(compiled => inca.souffle.backend.Executor.instantiate(compiled))
  }

  @main
  def runMicroDlViatra(): Unit = {
    runMicroDL(compiled => inca.viatra.Executor().instantiate(compiled))
  }

  @main
  def runMicroDlAscent(): Unit = {
    runMicroDL(compiled => inca.ascent.backend.Executor().instantiate(compiled))
  }


