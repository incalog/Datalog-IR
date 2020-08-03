package inca.trans.gp

import inca.IncaMatchers
import inca.analyzedLangs.Exp
import inca.analyzedLangs.Exp._
import inca.lang.fun.Fun.{Exp => _, _}
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import inca.runtime.index.dynamic.ParentIndex
import inca.trans.ExpLangTestAnalyses._
import org.scalatest.funsuite.AnyFunSuite
import truechange.{JavaLitType, SortType}

class CompileToPSystemTest extends AnyFunSuite with IncaMatchers {

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



  val scope = new QueryScope(langMetaInfo, Seq(new ParentIndex))

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

    assertMatch(module, "parent", mul, scope) { matcher =>
      assert(matcher.getAllMatches.size == 4)
    }
  }
}
