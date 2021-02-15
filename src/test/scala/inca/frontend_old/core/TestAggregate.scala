package inca.frontend_old.core

import inca.IncaMatchers
import inca.analyzedData.Nat._
import inca.analyzedLangs.Exp
import inca.compiler.Options
import inca.frontend_old.core.tree._
import inca.frontend_old.extensions.evalCall.Trees._
import inca.runtime.context.QueryScope
import inca.util.Meta.Scala
import org.scalatest.flatspec.AnyFlatSpec

import scala.language.implicitConversions
import scala.meta.XtensionQuasiquoteTerm

class TestAggregate extends AnyFlatSpec with IncaMatchers {
  val scope = new QueryScope(Exp.languageMetaInfo)
  val options = Options(scope.langMetaInfo)

  implicit def name(s: String): Name = Name(s)

  "aggregate" should "support non-invertible joins" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "1_to_10", Seq(), NatTyp, Seq(
        Body(Seq(Yield(EvalCall(succOp, Seq(Eval(Seq(), zeroOp)))))),
        Body(
          Seq(
            Assign(Seq("pred"), Call("1_to_10", Seq())),
            Assert(Eval(Seq(EvalParam("pred")), Scala(q"pred.toInt < 10"))),
            Yield(EvalCall(succOp, Seq(Var("pred"))))
          )
        )
      )),

      PatternFunction(None, "sum_1_to_10", Seq(), NatTyp, Seq(
        Body(Seq(
          Yield(Aggregate(Eval(Seq(), sumAggregation), Call("1_to_10", Seq()).typed(NatTyp)))
        ))
      ))
    ))

    val input = Exp.BooleanLit(true)

    assertMatchFunModule(module, "sum_1_to_10", input, scope) { matcher =>
      assert(matcher.getAllMatches.size() == 1)
      assert(matcher.getAllMatchArrays.head.head.asInstanceOf[Nat].toInt == (1 to 10).sum)
    }
  }

  "aggregate" should "support invertible joins" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "1_to_10", Seq(), NatTyp, Seq(
        Body(Seq(Yield(EvalCall(succOp, Seq(Eval(Seq(), zeroOp)))))),
        Body(
          Seq(
            Assign(Seq("pred"), Call("1_to_10", Seq())),
            Assert(Eval(Seq(EvalParam("pred")), Scala(q"pred.toInt < 10"))),
            Yield(EvalCall(succOp, Seq(Var("pred"))))
          )
        )
      )),

      PatternFunction(None, "sum_1_to_10", Seq(), NatTyp, Seq(
        Body(Seq(
          Yield(Aggregate(Eval(fastSumAggregation), Call("1_to_10", Seq()).typed(NatTyp)))
        ))
      ))
    ))

    val input = Exp.BooleanLit(true)

    assertMatchFunModule(module, "sum_1_to_10", input, scope) { matcher =>
      assert(matcher.getAllMatches.size() == 1)
      assert(matcher.getAllMatchArrays.head.head.asInstanceOf[Nat].toInt == (1 to 10).sum)
    }
  }

  "aggregate" should "support non-invertible joins 2" in {
    val module = Module("Test_Cast", Seq(), Seq(
      ScalaModuleContent(Scala(q"import inca.analyzedData.Nat.sumAgg")),
      ScalaModuleContent(Scala(q"val nine = 9")),
      ScalaModuleContent(Scala(q"object One { val num = 1 }")),
      PatternFunction(None, "1_to_10", Seq(), NatTyp, Seq(
        Body(Seq(Yield(EvalCall(succOp, Seq(Eval(Seq(), zeroOp)))))),
        Body(
          Seq(
            Assign(Seq("pred"), Call("1_to_10", Seq())),
            Assert(Eval(Scala(q"pred.toInt < (nine + One.num)"))),
            Yield(EvalCall(succOp, Seq(Var("pred"))))
          )
        )
      )),

      PatternFunction(None, "sum_1_to_10", Seq(), NatTyp, Seq(
        Body(Seq(
          Yield(Aggregate(Eval(Scala(q"sumAgg")), Call("1_to_10", Seq()).typed(NatTyp)))
        ))
      ))
    ))

    val input = Exp.BooleanLit(true)

    assertMatchFunModule(module, "sum_1_to_10", input, scope) { matcher =>
      assert(matcher.getAllMatches.size() == 1)
      assert(matcher.getAllMatchArrays.head.head.asInstanceOf[Nat].toInt == (1 to 10).sum)
    }
  }

}
