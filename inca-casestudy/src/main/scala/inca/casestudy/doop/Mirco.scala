package inca.casestudy.doop

import inca.ir.string2name
import inca.ir.CompiledModule
import inca.ir.execution.IRExecutor
import inca.souffle.frontend.compile.CompiledSouffleModule

import scala.io.Source

object Mirco:

  private def runMicroDL(createEngine: (compiled: CompiledModule) => IRExecutor#Engine) =
    val baseDir = "doop/"
    val source = Source.fromResource(baseDir + "micro.dl")
    val compiled = CompiledSouffleModule.fromSource("micro", source)

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
  def runMicroDlInca(): Unit = {
    runMicroDL(compiled => inca.viatra.Executor().instantiate(compiled))
  }

  // TODO: Produces a 500MB Ascent file... This does of course not compile aka compiles to slow
  //  The reason for this is, because we write edb facts directly into the file
  //  Figure out a way to read them from disk instead
  @main
  def runMicroDlAscent(): Unit = {
    runMicroDL(compiled => inca.ascent.backend.Executor.instantiate(compiled))
  }


