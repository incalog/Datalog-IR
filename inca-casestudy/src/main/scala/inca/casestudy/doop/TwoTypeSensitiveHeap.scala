package inca.casestudy.doop

import inca.ir.execution.ThreadCount.{Auto, Fixed}
import inca.ir.execution.{IRExecutor, ThreadCount, UnitRelation}
import inca.ir.extension.{block, bool, disjunction, module, not}
import inca.ir.optimize.AliasElimination
import inca.ir.{CompiledUnit, string2name}
import inca.souffle.frontend.compile.CompiledSouffleProgram
import inca.util.compileroptions.CompilerOptions
import inca.viatra.backend
import inca.viatra.backend.Executor

import scala.io.Source

// TODO: Links, but not yet executable since GenerateIR for Souffle is missing features
object TwoTypeSensitiveHeap:
  private def runContextInsensitiveDL(createEngine: (compiled: CompiledUnit) => IRExecutor#Engine, file: String = "2-type-sensitive+heap-flatten.dl"): Unit =
    val baseDir = "doop"
    val source = Source.fromResource(baseDir + "/" + file)
    val options = CompilerOptions.default
    //options.irLogging.logLowerings = true
    val compiled = CompiledSouffleProgram.fromSource("TwoTypeSensitiveHeap", source, options)
    compiled.setOptimizationPipeline(List())
    compiled.setPipeline(List(
      () => new bool.Lowering {},
      () => new block.Lowering {},
      () => new disjunction.Lowering {},
      () => new not.Lowering {},
      () => new AliasElimination {},
      () => new module.Lowering {}
    ))

    println(compiled.mainUnit.compiled)

  /*println("Load edb from files...")
  val edbFacts = compiled.loadEdbInputs(baseDir)
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
  }.sum*/

  /*val start = System.currentTimeMillis()
  engine.read(UnitRelation("VarPointsTo"))
  val end = System.currentTimeMillis()
  val execTime = end - start

  println(execTime / 1000.0)*/

  @main
  def runTwoTypeSensitiveHeapDL(): Unit = {
    runContextInsensitiveDL(
      compiled => inca.souffle.backend.Executor(Auto).instantiate(compiled)
    )
  }
