package inca.frontend.souffle

import inca.backend.analyze.DependencyGraph
import inca.compiler.source.SourceFile
import inca.frontend.souffle.lowering.SouffleToDatalogIR
import inca.frontend.souffle.lowering.SouffleToNamedRelations
import inca.frontend.souffle.parser.Parser
import inca.measurements.util.MemoryUtil
import inca.runtime.context.QueryScope
import inca.runtime.EnginePool
import java.io.File
import org.eclipse.viatra.query.runtime.rete.matcher.DRedReteBackendFactory

object MeasureDoopAnalysis extends App {
  val benchmarkPath = "souffle-frontend/benchmark"
  val file = new File(s"$benchmarkPath/self-contained.dl")
  val analysis = Parser.parse(SourceFile(file.toPath))
  val compiler = new SouffleToDatalogIR(false)
  val compiledModule = compiler.compile("selfcontained", analysis)
  println(compiledModule.ir.pats.size)
  println(compiledModule.ir.pats.map(_.bodies.size).sum)

  println(new DependencyGraph(compiledModule.optimized).toGraphViz)

  val psModule = compiledModule.psystemModule
  val startLoadFactFiles = System.currentTimeMillis()
  val inputCompiler = new SouffleToNamedRelations(s"$benchmarkPath/minijavac")
  val dbInput = inputCompiler.compile(compiledModule.inputs.values.map(x => x._2 -> x._1).toMap)
  val endLoadFactFiles = System.currentTimeMillis()
  println(s"Load fact files: ${endLoadFactFiles - startLoadFactFiles}ms")

  val RUNS = 1

  for (run <- 1 to RUNS) {
    val queryScope = new QueryScope(compiledModule.dataModel, Seq())
    val (engine, database) =
      EnginePool.loadEngineAndDatabase(queryScope, DRedReteBackendFactory.INSTANCE)

    val startQuery = System.currentTimeMillis()
    var loadingTime: Long = 0
    engine.delayUpdatePropagation { () =>
      val startLoadDB = System.currentTimeMillis()
      database.processDatabaseInput(dbInput)
      val endLoadDB = System.currentTimeMillis()
      loadingTime = endLoadDB - startLoadDB
    }
    val endQuery = System.currentTimeMillis()

    println(s"Run $run: Time to fill database: ${loadingTime}ms")
    println(s"Run $run: Used memory: ${MemoryUtil.usedMemoryInMBytes()}MB")
    println(s"Run $run: Time to process query: ${endQuery - startQuery}ms")

    //    matchers.foreach { m =>
    //      println(s"${m.getPatternName}: ${m.countMatches()}")
    //    }
    System.gc()
    Thread.sleep(500)
  }

  // INCREMENTAL UPDATES
  //
  //    val heapAllocationLoads = editScript.edits.collect { case l: Load => l}.filter { l => l.tag == NamedTag("_AssignHeapAllocation") }
  //    val invalidPrefixes = Seq("<sun", "<java", "<com")
  //    val impactfullLoads = heapAllocationLoads.filter { l =>
  //      val first = l.lits.head._2.asInstanceOf[String]
  //      !invalidPrefixes.exists(first.startsWith)
  //    }
  //
  //    val varPointsToMatcher = matchers.find(_.getPatternName.endsWith("VarPointsTo")).get
  //    val rnd = new Random(1563540296429L)
  //    val indices = impactfullLoads.indices.toBuffer
  //    Collections.shuffle(indices.asJava, rnd)
  //
  //    def measureEdit(edit: Edit): Unit = {
  //      val startReinsert = System.currentTimeMillis()
  //      database.processEdit(edit)
  //      val timeReinsert = System.currentTimeMillis() - startReinsert
  //      println(s"$timeReinsert\t${varPointsToMatcher.countMatches()}")
  //    }
  //
  //    indices.take(2000).foreach { index =>
  //      val load = impactfullLoads(index)
  //      val unload = Unload(load.node, load.tag, load.kids, load.lits)
  //      measureEdit(unload)
  //      measureEdit(load)
  //    }
}
