package inca.util.matchers

import inca.compiler.CompiledModule
import inca.compiler.Options
import inca.runtime.context.DataModel
import inca.runtime.context.QueryScope
import inca.runtime.db.DatabaseInput
import inca.runtime.EnginePool
import inca.runtime.Query
import org.scalatest.matchers.should.Matchers
import org.scalatest.Assertion
import truechange.EditScript

trait IncaMatchers extends Matchers {
  def scope: QueryScope

  def options: Options

  def dataModel: DataModel

  def assertMatch(
      module: CompiledModule,
      fun: String,
      editScript: EditScript
    )(
      asserter: Query.Matcher => Assertion
    ): Assertion = {

    val psystem = module.psystemModule
    val querySpec = psystem.patterns.getOrElse(
      fun,
      throw new IllegalArgumentException(s"Function $fun undefined in module ${module.name}.")
    )

    val feed = EnginePool.loadDatabase(scope, options.mode)
    val matcher =
      EnginePool.loadQuery(querySpec(), scope, options.mode)

    feed.processEditScript(editScript)

    try {
      asserter(matcher)
    } finally {
      EnginePool.disposeAllEngines()
    }
  }

  def assertMatch(
      module: CompiledModule,
      fun: String,
      dbInput: DatabaseInput
    )(
      asserter: Query.Matcher => Assertion
    ): Assertion = {

    val psystem = module.psystemModule
    val querySpec = psystem.patterns.getOrElse(
      fun,
      throw new IllegalArgumentException(s"Function $fun undefined in module ${module.name}.")
    )

    val feed = EnginePool.loadDatabase(scope, options.mode)
    val matcher =
      EnginePool.loadQuery(querySpec(), scope, options.mode)

    feed.processEditScript(dbInput.es)
    dbInput.insertions.foreach { case (rel, tuples) =>
      tuples.foreach(feed.insertExtensionalTuple(rel, _))
    }
    dbInput.deletions.foreach { case (rel, tuples) =>
      tuples.foreach(feed.deleteExtensionalTuple(rel, _))
    }

    try {
      asserter(matcher)
    } finally {
      EnginePool.disposeAllEngines()
    }
  }

}
