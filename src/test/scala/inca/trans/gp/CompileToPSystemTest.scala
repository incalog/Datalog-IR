package inca.trans.gp

import inca.AnalysisWriter
import inca.analyzedLangs.expLang._
import inca.lang.fun.Fun.{Exp => _, _}
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import inca.runtime.index.DynamicKey
import inca.runtime.index.dynamic.{DynamicIndex, ParentIndex}
import inca.runtime.{EnginePool, Query}
import inca.trans.ExpLangTestAnalyses._
import org.eclipse.viatra.query.runtime.api.{IPatternMatch, ViatraQueryMatcher}
import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import truechange.{JavaLitType, SortType}
import truediff.Diffable

class CompileToPSystemTest extends AnyFunSuite {

  private val testInput = Add(And(Or(BooleanLit(false), BooleanLit(false)), IntegerLit(5)), LongLit(10L))
  private val testInputNumericAddition = Add(Add(IntegerLit(5), IntegerLit(7)), Add(LongLit(7), IntegerLit(8)))

  val expType = SortType(classOf[Exp].getCanonicalName)
  val intTag = classOf[IntegerLit].getCanonicalName
  val intType = SortType(intTag)
  val longTag = classOf[LongLit].getCanonicalName
  val longType = SortType(longTag)
  val boolTag = classOf[BooleanLit].getCanonicalName
  val boolType = SortType(boolTag)
  val addTag = classOf[Add].getCanonicalName
  val addType = SortType(addTag)
  val multTag = classOf[Mult].getCanonicalName
  val multType = SortType(multTag)
  val andTag = classOf[And].getCanonicalName
  val andType = SortType(andTag)
  val orTag = classOf[Or].getCanonicalName
  val orType = SortType(orTag)
  val notTag = classOf[Not].getCanonicalName
  val notType = SortType(notTag)

  // TODO we need to derive this information but at this time we hardcode it
  private val langMetaInfo: LanguageMetaInfo =
    new LanguageMetaInfo(
      Map[SortType, Set[SortType]](
        expType -> Set(),
        intType -> Set(expType),
        longType -> Set(expType),
        boolType -> Set(expType),
        multType -> Set(expType),
        addType -> Set(expType),
        andType -> Set(expType),
        orType -> Set(expType),
        notType -> Set(expType),
      ),
      Map(
        (addTag->"lhs") -> expType,
        (addTag->"rhs") -> expType,
        (multTag->"lhs") -> expType,
        (multTag->"rhs") -> expType,
        (andTag->"lhs") -> expType,
        (andTag->"rhs") -> expType,
        (orTag->"lhs") -> expType,
        (orTag->"rhs") -> expType,
        (notTag->"e") -> expType
      ),
      Map(
        (intTag->"value") -> JavaLitType(classOf[java.lang.Integer]),
        (longTag->"value") -> JavaLitType(classOf[java.lang.Long]),
        (boolTag->"value") -> JavaLitType(classOf[java.lang.Boolean])
      ))

  def assertMatch(
      subjectProg: Diffable,
      compiledModuleClassname: String)(asserter: ViatraQueryMatcher[_ <: IPatternMatch] => Assertion): Assertion = {

    val editScript = Diffable.load(subjectProg)
    val virtualIndices = Map[DynamicKey, DynamicIndex](ParentIndex())
    val scope = new QueryScope(langMetaInfo, virtualIndices)

    val clazz = Class.forName("inca.trans.generated." + compiledModuleClassname)
    assert(clazz != null)

    val instanceMethod = clazz.getMethod("instance")
    val querySpec = instanceMethod.invoke(null).asInstanceOf[Query.Specification]

    val (feed, matcher) = EnginePool.loadQuery(querySpec, scope, DifferentialReteBackendFactory.INSTANCE)
    feed.processEditScript(editScript)

    try {
      asserter(matcher)
    } finally {
      EnginePool.disposeAllEngines()
    }
  }

  test("simple compare constraint") {
    val module = Module("Test", Seq(), Seq(idFun))
    AnalysisWriter.writeModule(module)

    assertMatch(testInputNumericAddition, "Test_idQuerySpecification") { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
    assertMatch(testInput, "Test_idQuerySpecification") { matcher =>
      assert(matcher.getAllMatches.size == 1)
    }
  }

  test("simple path constraint") {
    val module = Module("Test", Seq(), Seq(lhChildFun))
    AnalysisWriter.writeModule(module)

    assertMatch(testInputNumericAddition, "Test_lhChildQuerySpecification") { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
    assertMatch(testInput, "Test_lhChildQuerySpecification") { matcher =>
      assert(matcher.getAllMatches.size == 1)
    }
  }

  test("multiple bodies") {
    val module = Module("Test", Seq(), Seq(childrenFun))
    AnalysisWriter.writeModule(module)

    assertMatch(testInputNumericAddition, "Test_childrenQuerySpecification") { matcher =>
      assert(matcher.getAllMatches.size == 6)
    }
    assertMatch(testInput, "Test_childrenQuerySpecification") { matcher =>
      assert(matcher.getAllMatches.size == 2)
    }
  }

  test("non negative, non transtive call") {
    val module = Module("Test", Seq(), Seq(callLhChildFun, lhChildFun))
    AnalysisWriter.writeModule(module)

    assertMatch(testInputNumericAddition, "Test_callLhChildQuerySpecification") { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
    assertMatch(testInput, "Test_callLhChildQuerySpecification") { matcher =>
      assert(matcher.getAllMatches.size == 1)
    }
  }

  test("constraint concept") {
    val module = Module("Test", Seq(), Seq(instanceAddFun))
    AnalysisWriter.writeModule(module)

    assertMatch(testInputNumericAddition, "Test_instanceAddQuerySpecification") { matcher =>
      assert(matcher.getAllMatches.size == 1)
    }
    assertMatch(testInput, "Test_instanceAddQuerySpecification") { matcher =>
      assert(matcher.getAllMatches.size == 0)
    }
  }

  test("no type annotation for param") {
    val module = Module("Test", Seq(), Seq(noParamTypeFun))
    AnalysisWriter.writeModule(module)

    assertMatch(testInputNumericAddition, "Test_noParamTypeQuerySpecification") { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
  }

  test("primitive datatype output") {
    val module = Module("Test", Seq(), Seq(isBooleanFun))
    AnalysisWriter.writeModule(module)

    assertMatch(testInputNumericAddition, "Test_isBooleanQuerySpecification") { matcher =>
      assert(matcher.getAllMatches.size == 0)
    }
    assertMatch(testInput, "Test_isBooleanQuerySpecification") { matcher =>
      assert(matcher.getAllMatches.size == 2)
    }
  }

  test("virtual parent link") {
    val num1 = IntegerLit(1)
    val num2 = IntegerLit(2)
    val add = Add(num1, num2)
    val num3 = IntegerLit(3)
    val mul = Mult(num3, add)
    val expType = TNode(classOf[Exp].getCanonicalName)
    val parentFun = PatternFunction(
      None,
      "parent",
      Seq(Param("in", None)),
      Seq(AnnoParam(None, expType)),
      Seq(
        Body(
          Seq(
            Assign(Seq("p"), PathAccess(Var("in"), ParentLink).typed(TAnyLinked)),
            Assert(InstanceOf(Var("p"), expType)),
            Yield(Var("p"))))))

    val module = Module("Test", Seq(), Seq(parentFun))
    AnalysisWriter.writeModule(module)

    assertMatch(mul, "Test_parentQuerySpecification") { matcher =>
      assert(matcher.getAllMatches.size == 4)
    }
  }
}
