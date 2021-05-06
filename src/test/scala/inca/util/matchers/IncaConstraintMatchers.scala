package inca.util.matchers

import inca.compiler
import inca.compiler.options.ConstraintOptions
import inca.frontend.constraint.core.Module
import inca.runtime.Query
import org.scalatest.Assertion
import truediff.Diffable

trait IncaConstraintMatchers extends IncaMatchers {
  def options: ConstraintOptions

  def assertDesugar(core: Module,
                    sugared: Module,
                    options: ConstraintOptions = this.options): Unit = {
    val desugared = compiler.Compiler.compileConstraint(sugared, options).desugared
    assertResult(core)(desugared)
  }

  def assertMatch(module: Module,
                  fun: String,
                  subjectProg: Diffable)
                 (asserter: Query.Matcher => Assertion): Assertion = {
    val editScript = Diffable.load(subjectProg)
    val compiled = compiler.Compiler.compileConstraint(module, options)
    assertMatch(compiled, fun, editScript)(asserter)
  }

  def assertMatch(module: String,
                  fun: String,
                  subjectProg: Diffable)
                 (asserter: Query.Matcher => Assertion): Assertion = {
    val editScript = Diffable.load(subjectProg)
    val compiled = compiler.Compiler.compileConstraint(module, options)
    assertMatch(compiled, fun, editScript)(asserter)
  }
}
