package inca

import inca.backend.ir.GP
import inca.frontend.core.Core
import inca.frontend.core.Core.Module
import inca.runtime.context.QueryScope
import inca.runtime.{EnginePool, Query}
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import truechange.EditScript
import truediff.Diffable

trait IncaMatchers extends Matchers {
  val scope: QueryScope
  val options: CompilerOptions

  def assertDesugar(core: Module, sugared: Module, options: CompilerOptions = this.options): Unit = {
//    println(sugared + "\n" + "-- should desugar to --" + "\n" + core)

    val desugared = Compiler.compileFun(sugared, options).desugared
    assertResult(core)(desugared)
  }

  def assertOptimize(optimized: GP.Module, original: GP.Module): Unit = {
    assertResult(optimized)(Compiler.compileGP(original, options).optimized)
  }

  def assertMatchCoreProg(module: Core.Module,
                          fun: String,
                          subjectProg: Diffable,
                          scope: QueryScope = this.scope,
                          options: CompilerOptions = this.options)
                         (asserter: Query.Matcher => Assertion): Assertion = {

    val editScript = Diffable.load(subjectProg)
    assertMatchCoreEdit(module, fun, editScript, scope, options)(asserter)
  }

  def assertMatchCoreEdit(module: Core.Module,
                          fun: String,
                          editScript: EditScript,
                          scope: QueryScope = this.scope,
                          options: CompilerOptions = this.options)
                         (asserter: Query.Matcher => Assertion): Assertion = {

    val psystem = Compiler.compileFun(module, options).psystemModule
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

  def assertMatchGPProg(module: GP.Module,
                        fun: String,
                        subjectProg: Diffable,
                        scope: QueryScope = this.scope,
                        options: CompilerOptions = this.options)
                       (asserter: Query.Matcher => Assertion): Assertion = {

    val editScript = Diffable.load(subjectProg)
    assertMatchGPEdit(module, fun, editScript, scope, options)(asserter)
  }

  def assertMatchGPEdit(module: GP.Module,
                        fun: String,
                        editScript: EditScript,
                        scope: QueryScope = this.scope,
                        options: CompilerOptions = this.options)
                       (asserter: Query.Matcher => Assertion): Assertion = {

    val psystem = Compiler.compileGP(module, options).psystemModule
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
