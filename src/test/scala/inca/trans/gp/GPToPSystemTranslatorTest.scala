package inca.trans.gp

import java.util.Collections

import inca.analyzedLangs.expLang._
import org.eclipse.viatra.query.runtime.api.{IPatternMatch, ViatraQueryMatcher}
import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import truediff.Diffable
import inca.backend.indices.{EnginePool, LanguageMetaInfo, QueryScope, TFQuerySpecification}
import inca.lang.FunLang.{Alternative, AnnoParam, Module, Param, PathAccess, PatternFunction, Return, Var}
import inca.MetaElements.{NodeType, ParentLink}
import inca.backend.virtual.ParentIndex
import inca.trans.fun.FunToGPTranslator
import inca.trans.ExpLangTestAnalyses._
import inca.trans.generated._
import inca.util.AnalysisWriter
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

class GPToPSystemTranslatorTest extends AnyFunSuite {

  private val testInput = Add(And(Or(BooleanLit(false), BooleanLit(false)), IntegerLit(5)), LongLit(10L))
  private val testInputNumericAddition = Add(Add(IntegerLit(5), IntegerLit(7)), Add(LongLit(7), IntegerLit(8)))

  val expName = classOf[Exp].getCanonicalName
  val intName = classOf[IntegerLit].getCanonicalName
  val longName = classOf[LongLit].getCanonicalName
  val boolName = classOf[BooleanLit].getCanonicalName
  val addName = classOf[Add].getCanonicalName
  val multName = classOf[Mult].getCanonicalName
  val andName = classOf[And].getCanonicalName
  val orName = classOf[Or].getCanonicalName
  val notName = classOf[Not].getCanonicalName

  // TODO we need to derive this information but at this time we hardcode it
  private val langMetaInfo: LanguageMetaInfo =
    new LanguageMetaInfo(
      Map(
        expName -> Set[String](),
        intName -> Set(expName),
        longName -> Set(expName),
        boolName -> Set(expName),
        multName -> Set(expName),
        addName -> Set(expName),
        andName -> Set(expName),
        orName -> Set(expName),
        notName -> Set(expName),
      ),
      Map(
        addName -> Map(
          "lhs" -> expName,
          "rhs" -> expName,
        ),
        multName -> Map(
          "lhs" -> expName,
          "rhs" -> expName,
        ),
        andName -> Map(
          "lhs" -> expName,
          "rhs" -> expName,
        ),
        orName -> Map(
          "lhs" -> expName,
          "rhs" -> expName,
        ),
        notName -> Map("e" -> expName),
        intName -> Map("value" -> "java.lang.Integer"),
        longName -> Map("value" -> "java.lang.Long"),
        boolName -> Map("value" -> "java.lang.Boolean")
      ))


  def assertMatch(
      module: Module,
      subjectProg: Diffable,
      compiledModule: TFQuerySpecification)(asserter: ViatraQueryMatcher[IPatternMatch] => Assertion): Assertion = {
    val gp = FunToGPTranslator.transformModule(module)
    AnalysisWriter.writeModule(gp)
    val changeset = Diffable.load(subjectProg)
    val scope = new QueryScope(langMetaInfo, Map("parent" -> new ParentIndex()))

    val matcher = EnginePool.getMatcher(compiledModule, scope, DifferentialReteBackendFactory.INSTANCE)
    scope.getEngineContext.getBaseIndex.processChangeset(changeset)

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
      List(AnnoParam(None, NodeType(classOf[Exp].getCanonicalName))),
      List(
        Alternative(
          List(
            Return(PathAccess(Var("in"), Seq(ParentLink)))))))
    val module = Module("Test", Seq(), Seq(parentFun))
    assertMatch(module, mul, Test_parentQuerySpecification.instance()) { matcher =>
      assert(matcher.getAllMatches.size == 4)
    }
  }
}
