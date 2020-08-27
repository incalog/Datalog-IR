package inca.frontend.core

import inca.analyzedData.Nat._
import inca.analyzedLangs.Exp
import inca.frontend.core.Core._
import inca.frontend.extensions.DataOpCall
import inca.runtime.context.QueryScope
import inca.{CompilerOptions, IncaMatchers}
import org.scalatest.flatspec.AnyFlatSpec

object TestAggregate {

}

class TestAggregate extends AnyFlatSpec with IncaMatchers {
  val scope = new QueryScope(Exp.languageMetaInfo)
  val options = CompilerOptions(scope.langMetaInfo)


  "aggregate" should "support non-invertible joins" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "1_to_10", Seq(), Seq(AnnoParam(None, NatTyp)), Seq(
        Body(Seq(Yield(DataOpCall(succOp, Seq(DataOpCall(zeroOp, Seq()).typed(NatTyp))).typed(NatTyp)))),
        Body(
          Seq(
            Assign(Seq("pred"), Call("1_to_10", Seq()).typed(NatTyp)),
            Assert(Eval(Seq("pred"), TBool, "pred.toInt < 10")),
            Yield(DataOpCall(succOp, Seq(Var("pred").typed(NatTyp))).typed(NatTyp))
          )
        )
      )),

      PatternFunction(None, "sum_1_to_10", Seq(), Seq(AnnoParam(None, NatTyp)), Seq(
        Body(Seq(
          Yield(Aggregate(zeroOp, addOp, None, Call("1_to_10", Seq()).typed(NatTyp)))
        ))
      ))
    ))

    val input = Exp.BooleanLit(true)

    assertMatch(module, "sum_1_to_10", input, scope) { matcher =>
      assert(matcher.getAllMatches.size() == 1)
      assert(matcher.getAllMatchArrays.head.head.asInstanceOf[Nat].toInt == (1 to 10).sum)
    }
  }

  "aggregate" should "support invertible joins" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "1_to_10", Seq(), Seq(AnnoParam(None, NatTyp)), Seq(
        Body(Seq(Yield(DataOpCall(succOp, Seq(DataOpCall(zeroOp, Seq()).typed(NatTyp))).typed(NatTyp)))),
        Body(
          Seq(
            Assign(Seq("pred"), Call("1_to_10", Seq()).typed(NatTyp)),
            Assert(Eval(Seq("pred"), TBool, "pred.toInt < 10")),
            Yield(DataOpCall(succOp, Seq(Var("pred").typed(NatTyp))).typed(NatTyp))
          )
        )
      )),

      PatternFunction(None, "sum_1_to_10", Seq(), Seq(AnnoParam(None, NatTyp)), Seq(
        Body(Seq(
          Yield(Aggregate(zeroOp, addOp, Some(subOp), Call("1_to_10", Seq()).typed(NatTyp)))
        ))
      ))
    ))

    val input = Exp.BooleanLit(true)

    assertMatch(module, "sum_1_to_10", input, scope) { matcher =>
      assert(matcher.getAllMatches.size() == 1)
      assert(matcher.getAllMatchArrays.head.head.asInstanceOf[Nat].toInt == (1 to 10).sum)
    }
  }
}
