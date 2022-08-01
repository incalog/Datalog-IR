package inca.util.matchers

import inca.backend.ir.DatalogScala
import inca.compiler
import inca.runtime.db.DatabaseInput
import inca.runtime.EnginePool
import inca.runtime.Query
import org.scalatest.Assertion
import truechange.EditScript
import truediff.Diffable

trait IncaGPMatchers extends IncaMatchers {

  def assertOptimize(optimized: DatalogScala.Module, original: DatalogScala.Module): Unit = {
    assertResult(optimized)(compiler.Compiler.compileGP(original, dataModel, options).optimized)
  }

  def assertMatch(
                   module: DatalogScala.Module,
                   fun: String,
                   subjectProg: Diffable
    )(
      asserter: Query.Matcher => Assertion
    ): Assertion = {

    val editScript = Diffable.load(subjectProg)
    assertMatch(module, fun, editScript)(asserter)
  }

  def assertMatch(
                   module: DatalogScala.Module,
                   patName: String,
                   editScript: EditScript
    )(
      asserter: Query.Matcher => Assertion
    ): Assertion = {

    val psystem = compiler.Compiler.compileGP(module, dataModel, options).psystemModule
    val querySpec = psystem.patterns.getOrElse(
      patName,
      throw new IllegalArgumentException(s"Pattern $patName undefined in module ${module.name}.")
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
                   module: DatalogScala.Module,
                   patName: String,
                   dbInput: DatabaseInput
    )(
      asserter: Query.Matcher => Assertion
    ): Assertion = {

    val psystem = compiler.Compiler.compileGP(module, dataModel, options).psystemModule
    val querySpec = psystem.patterns.getOrElse(
      patName,
      throw new IllegalArgumentException(s"Pattern $patName undefined in module ${module.name}.")
    )

    val feed = EnginePool.loadDatabase(scope, options.mode)
    val matcher =
      EnginePool.loadQuery(querySpec(), scope, options.mode)

    feed.processDatabaseInput(dbInput)

    try {
      asserter(matcher)
    } finally {
      EnginePool.disposeAllEngines()
    }
  }
}
