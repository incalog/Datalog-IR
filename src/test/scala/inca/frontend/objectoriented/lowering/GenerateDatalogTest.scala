package inca.frontend.objectoriented.lowering

import inca.backend.analyze.DependencyGraph
import inca.backend.ir.util.printer.DatalogPrinter
import inca.backend.transform.magic.demand.DemandTransformation
import inca.backend.transform.objectoriented.AllocTransformation
import inca.compiler.Compiler
import inca.frontend.objectoriented.compiler.ObjectOptions
import inca.runtime.EnginePool
import inca.runtime.context.QueryScope
import inca.util.FileUtil
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite


class GenerateDatalogTest extends AnyFunSuite {

  val options: ObjectOptions = ObjectOptions()

  test("Base 1") {
    val code = FileUtil.readFile("objectoriented/unittests/Base1.oinca")
    val result = Compiler.compileObject(code, options).ir
    print(result)
  }

  test("Base 2") {
    val code = FileUtil.readFile("objectoriented/unittests/Base2.oinca")
    val result = Compiler.compileObject(code, options).ir
    print(result)
  }

  test("Base 3") {
    val code = FileUtil.readFile("objectoriented/unittests/Base3.oinca")
    val result = Compiler.compileObject(code, options).ir
    print(result)
  }

  test("Test") {
    val result = Compiler.compileObject("module Test", ObjectOptions())

    val graph = new DependencyGraph(result.transformed)
    println("Dependency graph")
    println(graph.toGraphViz)

    println()
    println("DatalogPrinter")
    println(DatalogPrinter.prettyModule(result.transformed)(verbose = true))
    println()

    val patterns = result.optimized.pats.map(_.name)
    val specs = patterns.map(result.psystemModule.patterns(_)()) // Nat$main // "Succ" for all Succ instances
    val scope = new QueryScope(result.dataModel)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    val matcher = specs.map(engine.getMatcher(_))
    feed.insert(DemandTransformation.demandPatternExtensionalPrefix + "main", Tuples.flatTupleOf())
    matcher.zip(patterns).foreach(m => println(m._2 +": " + m._1.getAllMatches.toArray.mkString(", ")))
  }

  test("Plus") {
    val code = FileUtil.readFile("objectoriented/unittests/Plus.oinca")
    val result = Compiler.compileObject(code, options)
    val spec = result.psystemModule.patterns("Nat$main")() // Nat$main // "Succ" for all Succ instances
    val scope = new QueryScope(result.dataModel)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    val mainMatcher = engine.getMatcher(spec)
    feed.insert(DemandTransformation.demandPatternExtensionalPrefix + "Nat$main", Tuples.flatTupleOf())
    mainMatcher.getAllMatches.toArray.foreach(println)
  }
}
