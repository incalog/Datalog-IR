package inca

import inca.frontend.fun.Fun.Module
import inca.frontend.funext.desugar.{Desugar, Desugarable}
import inca.runtime.context.QueryScope
import inca.runtime.{EnginePool, Query}
import inca.util.Meta
import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import truediff.Diffable

trait IncaMatchers extends Matchers {
  def assertDesugar(core: Module, sugared: Module, desugarables: Desugarable*): Unit = {
    println(sugared + "\n" + "-- should desugar to --" + "\n" + core)

    assertResult(core)(Desugar(desugarables:_*)(sugared))
  }

  def assertMatch( module: Module,
                   fun: String,
                   subjectProg: Diffable,
                   scope: QueryScope,
                   desugarables: Desugarable*
                 )(asserter: Query.Matcher => Assertion): Assertion = {

    val psystem = Meta.loadModule(module, desugarables:_*)
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
