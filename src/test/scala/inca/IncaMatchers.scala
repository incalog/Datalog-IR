package inca

import inca.backend.ir.GP
import inca.compiler.{CompiledModule, Options}
import inca.frontend.core.tree.Module
import inca.runtime.context.{DataModel, QueryScope}
import inca.runtime.{EnginePool, Query}
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import truechange.EditScript
import truediff.Diffable

trait IncaMatchers extends Matchers {
  val scope: QueryScope
  val options: Options
  val dataModel: DataModel

  def assertDesugar(core: Module, sugared: Module, options: Options = this.options): Unit = {
    val desugared = compiler.Compiler.compileFun(sugared, options).desugared
    assertResult(core)(desugared)
  }

  def assertOptimize(optimized: GP.Module, original: GP.Module): Unit = {
    assertResult(optimized)(compiler.Compiler.compileGP(original, dataModel, options).optimized)
  }

  def assertMatchFunCode (module: String,
                          fun: String,
                          subjectProg: Diffable,
                          scope: QueryScope = this.scope,
                          options: Options = this.options)
                         (asserter: Query.Matcher => Assertion): Assertion = {

    val editScript = Diffable.load(subjectProg)
    val compiled = compiler.Compiler.compileFun(module, options)
    assertMatchCoreEdit(compiled, fun, editScript, scope)(asserter)
  }

  def assertMatchFunModule(module: Module,
                           fun: String,
                           subjectProg: Diffable,
                           scope: QueryScope = this.scope,
                           options: Options = this.options)
                          (asserter: Query.Matcher => Assertion): Assertion = {

    val editScript = Diffable.load(subjectProg)
    val compiled = compiler.Compiler.compileFun(module, options)
    assertMatchCoreEdit(compiled, fun, editScript, scope)(asserter)
  }

  def assertMatchCoreEdit(module: CompiledModule,
                          fun: String,
                          editScript: EditScript,
                          scope: QueryScope = this.scope)
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

  def assertMatchGPProg(module: GP.Module,
                        fun: String,
                        subjectProg: Diffable,
                        scope: QueryScope = this.scope,
                        options: Options = this.options)
                       (asserter: Query.Matcher => Assertion): Assertion = {

    val editScript = Diffable.load(subjectProg)
    assertMatchGPEdit(module, fun, editScript, scope, options)(asserter)
  }

  def assertMatchGPEdit(module: GP.Module,
                        fun: String,
                        editScript: EditScript,
                        scope: QueryScope = this.scope,
                        options: Options = this.options)
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
