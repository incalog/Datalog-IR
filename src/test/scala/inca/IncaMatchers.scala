package inca

import inca.backend.ir.GP
import inca.frontend.desugar.Desugar
import inca.frontend.fun.Fun.Module
import inca.runtime.context.QueryScope
import inca.runtime.{EnginePool, Query}
import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import truediff.Diffable

trait IncaMatchers extends Matchers {
  val scope: QueryScope
  val options: CompilerOptions

  def assertDesugar(core: Module, sugared: Module, options: CompilerOptions = this.options): Unit = {
//    println(sugared + "\n" + "-- should desugar to --" + "\n" + core)

    assertResult(core)(Desugar(options.desugarables)(sugared))
  }

  def assertOptimize(optimized: GP.Module, original: GP.Module): Unit = {
    assertResult(optimized)(Compiler.optimize(original, options))
  }

  def assertMatch(module: Module,
                  fun: String,
                  subjectProg: Diffable,
                  scope: QueryScope = this.scope,
                  options: CompilerOptions = this.options)
                 (asserter: Query.Matcher => Assertion): Assertion = {

    val psystem = Compiler.compileAndLoadFunModule(module, None, options)
    val querySpec = psystem.patterns.getOrElse(fun, throw new IllegalArgumentException(s"Function $fun undefined in module ${module.name}."))

    val (feed, matcher) = EnginePool.loadQuery(querySpec(), scope, DifferentialReteBackendFactory.INSTANCE)

    val editScript = Diffable.load(subjectProg)
    feed.processEditScript(editScript)

    try {
      asserter(matcher)
    } finally {
      EnginePool.disposeAllEngines()
    }
  }
}
