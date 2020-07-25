package inca.trans.gp

import inca.analyzedLangs.expLang._
import inca.lang.fun.CompileToGP
import inca.lang.fun.Fun.{AnnoParam, Assert, Assignment, Body, InstanceOf, Module, Param, ParentLink, PathAccess, PatternFunction, Return, TNode, Var}
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import inca.runtime.index.MetaElements.{NamedLink, NodeType, PrimitiveType}
import inca.runtime.index.VirtualIndex
import inca.runtime.virtual.list.ListNextIndex
import inca.runtime.virtual.tree.ParentIndex
import inca.runtime.{EnginePool, IncaQuerySpecification}
import inca.trans.ExpLangTestAnalyses._
import inca.trans.generated._
import inca.util.AnalysisWriter
import org.eclipse.viatra.query.runtime.api.{IPatternMatch, ViatraQueryMatcher}
import org.eclipse.viatra.query.runtime.rete.matcher.DifferentialReteBackendFactory
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import truediff.Diffable

class CompileToPSystemTest extends AnyFunSuite {

  private val testInput = Add(And(Or(BooleanLit(false), BooleanLit(false)), IntegerLit(5)), LongLit(10L))
  private val testInputNumericAddition = Add(Add(IntegerLit(5), IntegerLit(7)), Add(LongLit(7), IntegerLit(8)))

  val expName = NodeType(classOf[Exp].getCanonicalName)
  val intName = NodeType(classOf[IntegerLit].getCanonicalName)
  val longName = NodeType(classOf[LongLit].getCanonicalName)
  val boolName = NodeType(classOf[BooleanLit].getCanonicalName)
  val addName = NodeType(classOf[Add].getCanonicalName)
  val multName = NodeType(classOf[Mult].getCanonicalName)
  val andName = NodeType(classOf[And].getCanonicalName)
  val orName = NodeType(classOf[Or].getCanonicalName)
  val notName = NodeType(classOf[Not].getCanonicalName)

  // TODO we need to derive this information but at this time we hardcode it
  private val langMetaInfo: LanguageMetaInfo =
    new LanguageMetaInfo(
      Map(
        expName -> Set(),
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
        NamedLink(addName, "lhs") -> expName,
        NamedLink(addName, "rhs") -> expName,
        NamedLink(multName, "lhs") -> expName,
        NamedLink(multName, "rhs") -> expName,
        NamedLink(andName, "lhs") -> expName,
        NamedLink(andName, "rhs") -> expName,
        NamedLink(orName, "lhs") -> expName,
        NamedLink(orName, "rhs") -> expName,
        NamedLink(notName, "e") -> expName,
        NamedLink(intName, "value") -> PrimitiveType("java.lang.Integer"),
        NamedLink(longName, "value") -> PrimitiveType("java.lang.Long"),
        NamedLink(boolName, "value") -> PrimitiveType("java.lang.Boolean")
      ))


  def assertMatch(
      module: Module,
      subjectProg: Diffable,
      compiledModule: IncaQuerySpecification)(asserter: ViatraQueryMatcher[_ <: IPatternMatch] => Assertion): Assertion = {
    val gp = CompileToGP.transformModule(module)
    AnalysisWriter.writeModule(gp)
    val changeset = Diffable.load(subjectProg)
    var virtualIndices = Seq[VirtualIndex]()
    val nextIndex = new ListNextIndex
    virtualIndices +:= nextIndex
    virtualIndices +:= new ParentIndex(nextIndex)
    val scope = new QueryScope(langMetaInfo, virtualIndices)

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
    val expType = TNode(classOf[Exp].getCanonicalName)
    val parentFun = PatternFunction(
      None,
      "parent",
      Seq(Param("in", None)),
      Seq(AnnoParam(None, expType)),
      Seq(
        Body(
          Seq(
            Assignment(Seq("p"), PathAccess(Var("in"), ParentLink)),
            Assert(InstanceOf(Var("p"), expType)),
            Return(Var("p"))))))
    val module = Module("Test", Seq(), Seq(parentFun))
    assertMatch(module, mul, Test_parentQuerySpecification.instance()) { matcher =>
      assert(matcher.getAllMatches.size == 4)
    }
  }
}
