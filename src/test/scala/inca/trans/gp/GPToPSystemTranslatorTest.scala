package inca.trans.gp

import inca.analyzedLangs.expLang.{Add, And, BooleanLit, Exp, IntegerLit, LongLit, Mult, Or}
import org.eclipse.viatra.query.runtime.api.{IPatternMatch, ViatraQueryMatcher}
import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import truediff.diffable.Diffable
import inca.backend.indices.{EnginePool, TFQueryScope, TFQuerySpecification}
import inca.lang.FunLang.{Alternative, AnnoParam, Module, Param, PathAccess, PatternFunction, Return, Var}
import inca.MetaElements.{NodeType, ParentLink}
import inca.trans.fun.FunToGPTranslator
import inca.trans.ExpLangTestAnalyses._
import inca.trans.generated.{Test_callLhChildQuerySpecification, Test_childrenQuerySpecification, Test_idQuerySpecification, Test_instanceAddQuerySpecification, Test_isBooleanQuerySpecification, Test_lhChildQuerySpecification, Test_noParamTypeQuerySpecification, Test_parentQuerySpecification}
import inca.util.AnalysisWriter
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

class GPToPSystemTranslatorTest extends AnyFunSuite {

  private val testInput = Add(And(Or(BooleanLit(false), BooleanLit(false)), IntegerLit(5)), LongLit(10L))
  private val testInputNumericAddition = Add(Add(IntegerLit(5), IntegerLit(7)), Add(LongLit(7), IntegerLit(8)))

  def assertMatch(
      module: Module,
      subjectProg: Diffable,
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

  test("no type annotation for param") {
    val module = Module("Test", Seq(), Seq(noParamTypeFun))
    assertMatch(module, testInputNumericAddition, Test_noParamTypeQuerySpecification.instance()) { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
    assertMatch(module, testInput, Test_noParamTypeQuerySpecification.instance()) { matcher =>
      assert(matcher.getAllMatches.size == 1)
    }
  }

  test("primitive datatype output") {
    val module = Module("Test", Seq(), Seq(isBooleanFun))
    assertMatch(module, testInputNumericAddition, Test_isBooleanQuerySpecification.instance()) { matcher =>
      assert(matcher.getAllMatches.size == 0)
    }
    assertMatch(module, testInput, Test_isBooleanQuerySpecification.instance()) { matcher =>
      assert(matcher.getAllMatches.size == 2)
    }
  }

  test("virtual parent link") {
    val num1 = IntegerLit(1)
    val num2 = IntegerLit(2)
    val add = Add(num1, num2)
    val num3 = IntegerLit(3)
    val mul = Mult(num3, add)
    val parentFun = PatternFunction(
      None,
      "parent",
      List(Param("in", None)),
      List(AnnoParam(None, NodeType(classOf[Exp]))),
      List(
        Alternative(
          List(
            Return(PathAccess(Var("in"), Seq(ParentLink())))))))
    val module = Module("Test", Seq(), Seq(parentFun))
    assertMatch(module, mul, Test_parentQuerySpecification.instance()) { matcher =>
      val indices = matcher.getEngine.getScope.asInstanceOf[TFQueryScope].getEngineContext.getBaseIndex
      indices.update(() => {
        indices.parentIndex.insertParent(num1, add)
        indices.parentIndex.insertParent(num2, add)
        indices.parentIndex.insertParent(num3, mul)
        indices.parentIndex.insertParent(add, mul)
      })
      println(matcher.getAllMatches())
      indices.update(() => {
        indices.parentIndex.deleteParent(num3, mul)
        indices.parentIndex.deleteParent(add, mul)
      })
      println(matcher.getAllMatches())
      assert(matcher.getAllMatches.size == 2)
    }

  }
}
