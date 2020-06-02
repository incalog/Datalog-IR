package org.inca.trans.gp

import org.eclipse.viatra.query.runtime.api.{IPatternMatch, ViatraQueryMatcher}
import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.inca.analyzedLangs.expLang._
import org.inca.generator.generated._
import org.inca.incer.Incrementalizable
import org.inca.incer.indices.{EnginePool, TFQueryScope, TFQuerySpecification}
import org.inca.lang.FunLang.Module
import org.inca.trans.ExpLangTestAnalyses._
import org.inca.trans.fun.FunToGPTranslator
import org.inca.util.AnalysisWriter
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

class GPToPSystemTranslatorTest extends AnyFunSuite {

  private val testInput = Add(And(Or(BooleanLit(false), BooleanLit(false)), IntegerLit(5)), LongLit(10L))
  private val testInputNumericAddition = Add(Add(IntegerLit(5), IntegerLit(7)), Add(LongLit(7), IntegerLit(8)))

  def assertMatch(
      module: Module,
      subjectProg: Incrementalizable,
      compiledModule: TFQuerySpecification)(asserter: ViatraQueryMatcher[IPatternMatch] => Assertion): Assertion = {
    val gp = FunToGPTranslator.transformModule(module)
//    println(GraphPatternLangPrinter.prettyModule(gp))
    AnalysisWriter.writeModule(gp)
    val scope = new TFQueryScope(subjectProg)
    val matcher = EnginePool.getMatcher(compiledModule, scope, DifferentialReteBackendFactory.INSTANCE)
    val result = asserter(matcher)
    EnginePool.disposeAllEngines()
    result
  }

  test("simple compare constraint") {
    val module = Module("Test", Seq(), Seq(idFun))
    assertMatch(module, testInputNumericAddition, Test_idQuerySpecification.instance()) { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
    assertMatch(module, testInput, Test_idQuerySpecification.instance()) { matcher =>
      assert(matcher.getAllMatches.size == 1)
    }
  }

  test("simple path constraint") {
    val module = Module("Test", Seq(), Seq(lhChildFun))
    assertMatch(module, testInputNumericAddition, Test_lhChildQuerySpecification.instance()) { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
    assertMatch(module, testInput, Test_lhChildQuerySpecification.instance()) { matcher =>
      assert(matcher.getAllMatches.size == 1)
    }
  }

  test("multiple bodies") {
    val module = Module("Test", Seq(), Seq(childrenFun))
    assertMatch(module, testInputNumericAddition, Test_childrenQuerySpecification.instance()) { matcher =>
      assert(matcher.getAllMatches.size == 6)
    }
    assertMatch(module, testInput, Test_childrenQuerySpecification.instance()) { matcher =>
      assert(matcher.getAllMatches.size == 2)
    }
  }

  test("non negative, non transtive call") {
    val module = Module("Test", Seq(), Seq(callLhChildFun, lhChildFun))
    assertMatch(module, testInputNumericAddition, Test_callLhChildQuerySpecification.instance()) { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
    assertMatch(module, testInput, Test_callLhChildQuerySpecification.instance()) { matcher =>
      assert(matcher.getAllMatches.size == 1)
    }
  }

  test("constraint concept") {
    val module = Module("Test", Seq(), Seq(instanceAddFun))
    assertMatch(module, testInputNumericAddition, Test_instanceAddQuerySpecification.instance()) { matcher =>
      assert(matcher.getAllMatches.size == 1)
    }
    assertMatch(module, testInput, Test_instanceAddQuerySpecification.instance()) { matcher =>
      assert(matcher.getAllMatches.size == 0)
    }
  }
}
