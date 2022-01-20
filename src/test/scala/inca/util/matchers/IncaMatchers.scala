package inca.util.matchers

import inca.compiler.{CompiledModule, Options}
import inca.runtime.context.{DataModel, QueryScope}
import inca.runtime.{EnginePool, Query}
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import truechange.EditScript

trait IncaMatchers extends Matchers {
  def scope: QueryScope

  def options: Options

  def dataModel: DataModel

  def assertMatch(module: CompiledModule,
                  fun: String,
                  editScript: EditScript)
                 (asserter: Query.Matcher => Assertion): Assertion = {

    val psystem = module.psystemModule
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
