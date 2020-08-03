package inca.lang.funext

import inca.lang.fun.Fun.Module
import inca.lang.funext.desugar.{Desugar, Desugarable}
import org.scalatest.matchers.should.Matchers

trait DesugarMatchers extends Matchers {
  def assertDesugar(core: Module, sugared: Module, desugarables: Desugarable*): Unit = {
    println(sugared + "\n" + "-- should desugar to --" + "\n" + core)

    assertResult(core)(Desugar(desugarables:_*)(sugared))
  }
}
