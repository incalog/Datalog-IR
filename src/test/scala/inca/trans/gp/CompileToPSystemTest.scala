package inca.trans.gp

import inca.IncaMatchers
import inca.analyzedLangs.Exp
import inca.analyzedLangs.Exp._
import inca.lang.fun.Fun.{Exp => _, _}
import inca.runtime.context.QueryScope
import inca.trans.ExpLangTestAnalyses._
import org.scalatest.funsuite.AnyFunSuite

class CompileToPSystemTest extends AnyFunSuite with IncaMatchers {

  private val testInput = Add(And(Or(BooleanLit(false), BooleanLit(false)), IntegerLit(5)), LongLit(10L))
  private val testInputNumericAddition = Add(Add(IntegerLit(5), IntegerLit(7)), Add(LongLit(7), IntegerLit(8)))

  val scope = new QueryScope(Exp.languageMetaInfo)

  test("simple compare constraint") {
    val module = Module("Test", Seq(), Seq(idFun))

    assertMatch(module, "id", testInputNumericAddition, scope) { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
    assertMatch(module, "id", testInput, scope) { matcher =>
      assert(matcher.getAllMatches.size == 1)
    }
  }

  test("simple path constraint") {
    val module = Module("Test", Seq(), Seq(lhChildFun))

    assertMatch(module, "lhChild", testInputNumericAddition, scope) { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
    assertMatch(module, "lhChild", testInput, scope) { matcher =>
      assert(matcher.getAllMatches.size == 1)
    }
  }

  test("multiple bodies") {
    val module = Module("Test", Seq(), Seq(childrenFun))

    assertMatch(module, "children", testInputNumericAddition, scope) { matcher =>
      assert(matcher.getAllMatches.size == 6)
    }
    assertMatch(module, "children", testInput, scope) { matcher =>
      assert(matcher.getAllMatches.size == 2)
    }
  }

  test("non negative, non transtive call") {
    val module = Module("Test", Seq(), Seq(callLhChildFun, lhChildFun))

    assertMatch(module, "callLhChild", testInputNumericAddition, scope) { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
    assertMatch(module, "callLhChild", testInput, scope) { matcher =>
      assert(matcher.getAllMatches.size == 1)
    }
  }

  test("constraint concept") {
    val module = Module("Test", Seq(), Seq(instanceAddFun))

    assertMatch(module, "instanceAdd", testInputNumericAddition, scope) { matcher =>
      assert(matcher.getAllMatches.size == 1)
    }
    assertMatch(module, "instanceAdd", testInput, scope) { matcher =>
      assert(matcher.getAllMatches.size == 0)
    }
  }

  test("no type annotation for param") {
    val module = Module("Test", Seq(), Seq(noParamTypeFun))

    assertMatch(module, "noParamType", testInputNumericAddition, scope) { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
  }

  test("primitive datatype output") {
    val module = Module("Test", Seq(), Seq(isBooleanFun))

    assertMatch(module, "isBoolean", testInputNumericAddition, scope) { matcher =>
      assert(matcher.getAllMatches.size == 0)
    }
    assertMatch(module, "isBoolean", testInput, scope) { matcher =>
      assert(matcher.getAllMatches.size == 2)
    }
  }

  test("virtual parent link") {
    val num1 = IntegerLit(1)
    val num2 = IntegerLit(2)
    val add = Add(num1, num2)
    val num3 = IntegerLit(3)
    val mul = Mul(num3, add)
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

    assertMatch(module, "parent", mul, scope) { matcher =>
      assert(matcher.getAllMatches.size == 4)
    }
  }
}
