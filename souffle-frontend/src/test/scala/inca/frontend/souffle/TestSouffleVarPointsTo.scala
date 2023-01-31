package inca.frontend.souffle

import inca.compiler.source.SourceFile
import inca.frontend.souffle.lowering.SouffleToDatalogIR
import inca.frontend.souffle.lowering.SouffleToNamedRelations
import inca.frontend.souffle.parser.Parser
import inca.runtime.context.QueryScope
import inca.runtime.EnginePool
import inca.runtime.Query.Matcher
import java.io.File
import org.eclipse.viatra.query.runtime.rete.matcher.DRedReteBackendFactory
import org.scalatest.flatspec.AnyFlatSpec

class TestSouffleVarPointsTo extends AnyFlatSpec {
  val expectedTupleCount = Map(
    "selfcontained_basic_SubtypeOf" -> 13487,
    "selfcontained_basic_SupertypeOf" -> 13487,
    "selfcontained_VarPointsTo0" -> 34291,
    "selfcontained_VarPointsTo" -> 511391,
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
    val benchmarkPath = "souffle-frontend/benchmark"
    val file = new File(s"$benchmarkPath/self-contained.dl")
    val analysis = Parser.parse(SourceFile(file.toPath))
    val compiler = new SouffleToDatalogIR(false)
    val compiledModule = compiler.compile("selfcontained", analysis)
    val psModule = compiledModule.psystemModule
    val inputCompiler = new SouffleToNamedRelations(s"$benchmarkPath/minijavac")
    val dbInput = inputCompiler.compile(compiledModule.inputs.values.map(x => x._2 -> x._1).toMap)

    val queryScope = new QueryScope(compiledModule.dataModel, Seq())
    val (engine, database) =
      EnginePool.loadEngineAndDatabase(queryScope, DRedReteBackendFactory.INSTANCE)

    def getMatcher(fun: String): Matcher = {
      val querySpec = psModule.patterns.getOrElse(
        fun,
        throw new IllegalArgumentException(s"Function $fun undefined in module.")
      )
      EnginePool.loadQuery(
        querySpec(),
        queryScope,
        DRedReteBackendFactory.INSTANCE
      )
    }

    val matchers = compiledModule.printSizes.map(ps => getMatcher(ps.name.name))

    engine.delayUpdatePropagation { () =>
      database.processDatabaseInput(dbInput)
    }

    matchers.foreach { m =>
      assert(m.countMatches() == expectedTupleCount(m.getPatternName))
    }

  }
}
