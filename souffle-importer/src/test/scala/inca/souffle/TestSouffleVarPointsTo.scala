package inca.souffle

import inca.runtime.EnginePool
import inca.runtime.Query.Matcher
import inca.runtime.context.QueryScope
import org.eclipse.viatra.query.runtime.rete.matcher.{DRedReteBackendFactory, TimelyReteBackendFactory}
import org.scalatest.flatspec.AnyFlatSpec
import truechange.EditScript

import scala.io.Source

class TestSouffleVarPointsTo extends AnyFlatSpec {
  val expectedTupleCount = Map(
    "selfcontained_basic_SubtypeOf" -> 13487,
    "selfcontained_basic_SupertypeOf" -> 13487,
    "selfcontained_VarPointsTo0" -> 34291,
    "selfcontained_VarPointsTo" -> 511391,
    "selfcontained_VarPointsTo0" -> 34291,
    "selfcontained_Assign" -> 31754,
    "selfcontained_Assign0" -> 19494,
    "selfcontained_InterProc0" -> 190648,
    "selfcontained_InterProc1" -> 1719,
    "selfcontained_InterProc2" -> 11101,
    "selfcontained_StaticFieldPointsTo" -> 705,
    "selfcontained_InstanceFieldPointsTo" -> 354511,
    "selfcontained_Reachable" -> 2875,
    "selfcontained_CallGraphEdge" -> 13136,
    "selfcontained_ArrayIndexPointsTo" -> 5636
  )

  "var points to souffle analysis" should "derive correct number of tuples" in {
    println(System.getProperty("user.dir"))
    val benchmarkPath = "souffle-importer/benchmark"
    val filename = s"$benchmarkPath/self-contained.dl"
    val src = Source.fromFile(filename)
    val doopText = src.getLines().mkString("\n")
    val analysis = Parser(doopText)
    src.close()
    val compiler = new SouffleToIncaCompiler
    val compiledModule = compiler.compile("selfcontained", analysis)

    val psModule = compiledModule.psystemModule
    val startLoadFactFiles = System.currentTimeMillis()
    val edits = compiledModule.inputs.flatMap { case (sig, input) =>
      val inputCompiler = new SouffleInputToEditscript(s"$benchmarkPath/minijavac")
      val editScript = inputCompiler.compile(input, sig)
      editScript.edits
    }
    val endLoadFactFiles = System.currentTimeMillis()
    println(s"Load fact files: ${endLoadFactFiles-startLoadFactFiles}ms")

    val editScript = EditScript(edits)
    println(editScript.size)


    val RUNS = 5

    for (run <- 1 to RUNS) {
      val queryScope = new QueryScope(compiledModule.dataModel, Seq())
      val (engine, database) = EnginePool.loadEngineAndDatabase(queryScope, DRedReteBackendFactory.INSTANCE)

      def getMatcher(fun: String): Matcher = {
        val querySpec = psModule.patterns.getOrElse(fun, throw new IllegalArgumentException(s"Function $fun undefined in module."))
        EnginePool.loadQuery(querySpec(), queryScope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
      }

      val matchers = compiledModule.printSizes.map(ps => getMatcher(ps.name))

      val startQuery = System.currentTimeMillis()
      var loadingTime: Long = 0
      engine.delayUpdatePropagation { () =>
        val startLoadDB = System.currentTimeMillis()
        database.processEditScript(editScript)
        val endLoadDB = System.currentTimeMillis()
        loadingTime = endLoadDB - startLoadDB
      }
      val endQuery = System.currentTimeMillis()

      matchers.foreach { m =>
        assert(m.countMatches() == expectedTupleCount(m.getPatternName))
      }

      println(s"Run $run: Time to fill database: ${loadingTime}ms")
      println(s"Run $run: Used memory: ${MeasurementUtils.usedMemoryInMBytes()}MB")
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
}
