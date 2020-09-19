package inca.backend.ir

import inca.analyzedLangs.Exp
import inca.analyzedLangs.Exp._
import inca.analyzedLangs.ExpLangTestAnalyses._
import inca.frontend.core.Core.{Exp => _, _}
import inca.runtime.context.QueryScope
import inca.{CompilerOptions, IncaMatchers}
import org.scalatest.funsuite.AnyFunSuite

class CompileToPSystemTest extends AnyFunSuite with IncaMatchers {

  private val testInput = Add(And(Or(BooleanLit(false), BooleanLit(false)), IntegerLit(5)), LongLit(10L))
  private val testInputNumericAddition = Add(Add(IntegerLit(5), IntegerLit(7)), Add(LongLit(7), IntegerLit(8)))

  val scope: QueryScope = new QueryScope(Exp.languageMetaInfo)
  val options: CompilerOptions = CompilerOptions(scope.langMetaInfo)

  test("simple compare constraint") {
    val module = Module("Test", Seq(), Seq(idFun))

    assertMatchCoreProg(module, "id", testInputNumericAddition) { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
    assertMatchCoreProg(module, "id", testInput) { matcher =>
      assert(matcher.getAllMatches.size == 1)
    }
  }

  test("simple path constraint") {
    val module = Module("Test", Seq(), Seq(lhChildFun))

    assertMatchCoreProg(module, "lhChild", testInputNumericAddition) { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
    assertMatchCoreProg(module, "lhChild", testInput) { matcher =>
      assert(matcher.getAllMatches.size == 1)
    }
  }

  test("multiple bodies") {
    val module = Module("Test", Seq(), Seq(childrenFun))

    assertMatchCoreProg(module, "children", testInputNumericAddition) { matcher =>
      assert(matcher.getAllMatches.size == 6)
    }
    assertMatchCoreProg(module, "children", testInput) { matcher =>
      assert(matcher.getAllMatches.size == 2)
    }
  }

  test("non negative, non transtive call") {
    val module = Module("Test", Seq(), Seq(callLhChildFun, lhChildFun))

    assertMatchCoreProg(module, "callLhChild", testInputNumericAddition) { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
    assertMatchCoreProg(module, "callLhChild", testInput) { matcher =>
      assert(matcher.getAllMatches.size == 1)
    }
  }

  test("constraint concept") {
    val module = Module("Test", Seq(), Seq(instanceAddFun))

    assertMatchCoreProg(module, "instanceAdd", testInputNumericAddition) { matcher =>
      assert(matcher.getAllMatches.size == 1)
    }
    assertMatchCoreProg(module, "instanceAdd", testInput) { matcher =>
      assert(matcher.getAllMatches.size == 0)
    }
  }

  test("no type annotation for param") {
    val module = Module("Test", Seq(), Seq(noParamTypeFun))

    assertMatchCoreProg(module, "noParamType", testInputNumericAddition) { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
  }

  test("primitive datatype output") {
    val module = Module("Test", Seq(), Seq(isBooleanFun))

    assertMatchCoreProg(module, "isBoolean", testInputNumericAddition) { matcher =>
      assert(matcher.getAllMatches.size == 0)
    }
    assertMatchCoreProg(module, "isBoolean", testInput) { matcher =>
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
      Seq(Param("in", TAny)),
      Seq(AnnoParam(None, expType)),
      Seq(
        Body(
          Seq(
            Assign(Seq("p"), PathAccess(Var("in").typed(TAny), ParentLink).typed(TAnyLinked)),
            Assert(InstanceOf(Var("p"), expType)),
            Yield(Var("p"))))))

    val module = Module("Test", Seq(), Seq(parentFun))

    assertMatchCoreProg(module, "parent", mul) { matcher =>
      assert(matcher.getAllMatches.size == 4)
    }
  }
}
