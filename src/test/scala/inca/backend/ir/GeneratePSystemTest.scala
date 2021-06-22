package inca.backend.ir

import inca.analyzedLangs.Exp
import inca.analyzedLangs.Exp._
import inca.analyzedLangs.ExpLangTestAnalyses._
import inca.frontend.constraint.compiler.ConstraintOptions
import inca.frontend.constraint.core._
import inca.runtime.context.{DataModel, QueryScope}
import inca.util.Scala
import inca.util.matchers.{IncaConstraintMatchers, IncaGPMatchers}
import org.scalatest.funsuite.AnyFunSuite

import scala.meta._

class GeneratePSystemTest extends AnyFunSuite with IncaGPMatchers with IncaConstraintMatchers {

  private val testInput = Add(And(Or(BooleanLit(false), BooleanLit(false)), IntegerLit(5)), LongLit(10L))
  private val testInputNumericAddition = Add(Add(IntegerLit(5), IntegerLit(7)), Add(LongLit(7), IntegerLit(8)))

  val scope: QueryScope = new QueryScope(Exp.model)
  val options: ConstraintOptions = ConstraintOptions()
  val dataModel: DataModel = Exp.model

  test("simple compare constraint") {
    val module = Module("Test", Seq(DirectDataModel(Exp.model)), Seq(), Seq(), Seq(idFun))

    assertMatch(module, "id", testInputNumericAddition) { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
    assertMatch(module, "id", testInput) { matcher =>
      assert(matcher.getAllMatches.size == 1)
    }
  }

  test("simple path constraint") {
    val module = Module("Test", Seq(DirectDataModel(Exp.model)), Seq(), Seq(), Seq(lhChildFun))

    assertMatch(module, "lhChild", testInputNumericAddition) { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
    assertMatch(module, "lhChild", testInput) { matcher =>
      assert(matcher.getAllMatches.size == 1)
    }
  }

  test("multiple bodies") {
    val module = Module("Test", Seq(DirectDataModel(Exp.model)), Seq(), Seq(), Seq(childrenFun))

    assertMatch(module, "children", testInputNumericAddition) { matcher =>
      assert(matcher.getAllMatches.size == 6)
    }
    assertMatch(module, "children", testInput) { matcher =>
      assert(matcher.getAllMatches.size == 2)
    }
  }

  test("non negative, non transtive call") {
    val module = Module("Test", Seq(DirectDataModel(Exp.model)), Seq(), Seq(), Seq(callLhChildFun, lhChildFun))

    assertMatch(module, "callLhChild", testInputNumericAddition) { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
    assertMatch(module, "callLhChild", testInput) { matcher =>
      assert(matcher.getAllMatches.size == 1)
    }
  }

  test("constraint concept") {
    val module = Module("Test", Seq(DirectDataModel(Exp.model)), Seq(), Seq(), Seq(instanceAddFun))

    assertMatch(module, "instanceAdd", testInputNumericAddition) { matcher =>
      assert(matcher.getAllMatches.size == 1)
    }
    assertMatch(module, "instanceAdd", testInput) { matcher =>
      assert(matcher.getAllMatches.size == 0)
    }
  }

  test("no type annotation for param") {
    val module = Module("Test", Seq(DirectDataModel(Exp.model)), Seq(), Seq(), Seq(noParamTypeFun))

    assertMatch(module, "noParamType", testInputNumericAddition) { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
  }

  test("primitive datatype output") {
    val module = Module("Test", Seq(DirectDataModel(Exp.model)), Seq(), Seq(), Seq(isBooleanFun))

    assertMatch(module, "isBoolean", testInputNumericAddition) { matcher =>
      assert(matcher.getAllMatches.size == 0)
    }
    assertMatch(module, "isBoolean", testInput) { matcher =>
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
      Seq(MainFunctionAnno),
      None,
      "parent",
      Seq(Param("in", TAny)),
      expType,
      Seq(
        Body(
          Seq(
            Assign(Seq("p"), PathAccess(Var("in"), ParentLink)),
            Assert(InstanceOf(Var("p"), expType)),
            Yield(Cast(Var("p"), expType))))))

    val module = Module("Test", Seq(DirectDataModel(Exp.model)), Seq(), Seq(), Seq(parentFun))

    assertMatch(module, "parent", mul) { matcher =>
      assert(matcher.getAllMatches.size == 4)
    }
  }

  test("unbounded literal parameter determined by eval") {
    val module = Datalog.Module("test_eval", Seq(),
      Seq(Datalog.Pattern(None, "intToString", Seq(Datalog.Param("exp", Datalog.TNode(Exp.intTag)), Datalog.Param("str", Datalog.TScalaString)),
        Seq(Datalog.Body(Seq(
          Datalog.Path(Datalog.Var("exp"), Datalog.TNode(Exp.intTag), Datalog.NamedLink(Datalog.TNode(Exp.intTag), "value"), Datalog.Var("value"), Datalog.TLiteral.Int),
          Datalog.Computed(Datalog.Var("str"), Datalog.Evaluation(Seq((Datalog.Var("value"), Datalog.TLiteral.Int)), Datalog.TScalaString, Scala(q"(value: Int) => value.toString"))))
        )))), Seq())
    assertMatch(module, "intToString", testInputNumericAddition) { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
  }

  test("unbounded argument of second eval") {
    val module = Datalog.Module("test_eval", Seq(),
      Seq(Datalog.Pattern(None, "intToString", Seq(Datalog.Param("exp", Datalog.TNode(Exp.intTag)), Datalog.Param("str2", Datalog.TScalaString)),
        Seq(Datalog.Body(Seq(
          Datalog.Path(Datalog.Var("exp"), Datalog.TNode(Exp.intTag), Datalog.NamedLink(Datalog.TNode(Exp.intTag), "value"), Datalog.Var("value"), Datalog.TLiteral.Int),
          Datalog.Computed(Datalog.Var("str"), Datalog.Evaluation(Seq((Datalog.Var("value"), Datalog.TLiteral.Int)), Datalog.TScalaString, Scala(q"(value: Int) => value.toString"))),
          Datalog.Computed(Datalog.Var("str2"), Datalog.Evaluation(Seq((Datalog.Var("str"), Datalog.TScalaString)), Datalog.TScalaString, Scala(q"""(str: String) => str + "_appended" """))))
        )))), Seq())
    assertMatch(module, "intToString", testInputNumericAddition) { matcher =>
//      println(matcher.getAllMatches)
      assert(matcher.getAllMatches.size == 3)
    }
  }
}
