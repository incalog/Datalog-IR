package inca.util.matchers

import inca.backend.ir.IR
import inca.compiler
import inca.runtime.{EnginePool, Query}
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.scalatest.Assertion
import truechange.EditScript
import truediff.Diffable

trait IncaGPMatchers extends IncaMatchers {

  def assertOptimize(optimized: IR.Module, original: IR.Module): Unit = {
    assertResult(optimized)(compiler.Compiler.compileGP(original, dataModel, options).optimized)
  }

  def assertMatch(module: IR.Module,
                  fun: String,
                  subjectProg: Diffable)
                 (asserter: Query.Matcher => Assertion): Assertion = {

    val editScript = Diffable.load(subjectProg)
    assertMatch(module, fun, editScript)(asserter)
  }

  def assertMatch(module: IR.Module,
                  fun: String,
                  editScript: EditScript)
                 (asserter: Query.Matcher => Assertion): Assertion = {

    val psystem = compiler.Compiler.compileGP(module, dataModel, options).psystemModule
    val querySpec = psystem.patterns.getOrElse(fun, throw new IllegalArgumentException(s"Function $fun undefined in module ${module.name}."))


    val feed = EnginePool.loadDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    val matcher = EnginePool.loadQuery(querySpec(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

    feed.processEditScript(editScript)

    try {
      asserter(matcher)
    } finally {
      EnginePool.disposeAllEngines()
    }
  }
}
