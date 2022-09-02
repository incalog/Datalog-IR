package inca.frontend.objectoriented.lowering

import inca.backend.transform.magic.demand.DemandTransformation
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

  test("Example") {
    val code = FileUtil.readFile("objectoriented/unittests/Example.oinca")
    val result = Compiler.compileObject(code, options).ir
    println(result)
  }

  test("Binary Tree 3") {
    val code = FileUtil.readFile("objectoriented/unittests/BinaryTree3.oinca")
    val result = Compiler.compileObject(code, options).ir
    println(result)
  }

  test("BinaryTree 2") {
    val code = FileUtil.readFile("objectoriented/unittests/BinaryTree2.oinca")
    val result = Compiler.compileObject(code, options).ir
    println(result)
  }
}
