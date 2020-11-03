package inca.frontend.extensions

import inca.IncaMatchers
import inca.analyzedData.Nat._
import inca.analyzedLangs.Exp
import inca.compiler.Options
import inca.frontend.core._
import inca.runtime.context.QueryScope
import org.scalatest.flatspec.AnyFlatSpec


class TestData extends AnyFlatSpec with IncaMatchers {
  val scope = new QueryScope(Exp.languageMetaInfo)
  val options = Options(scope.langMetaInfo)

  implicit def name(s: String): Name = Name(s)

  "data op" should "support zero-arg data op" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "zero", Seq(), Seq(AnnoParam(None, TBool)), Seq(
        Body(Seq(Yield(EvalCall(zeroOp.asScala, Seq()))))
      ))
    ))

    val input = Exp.BooleanLit(true)

    assertMatchCoreProg(module, "zero", input, scope) { matcher =>
      assert(matcher.getAllMatches.size() == 1)
    }
  }

  "data op" should "support non-zero-arg data op" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "one", Seq(), Seq(AnnoParam(None, TBool)), Seq(
        Body(Seq(Yield(EvalCall(succOp.asScala, Seq(EvalCall(zeroOp.asScala, Seq()))))))
      ))
    ))

    val input = Exp.BooleanLit(true)

    assertMatchCoreProg(module, "one", input, scope) { matcher =>
      assert(matcher.getAllMatches.size() == 1)
    }
  }

//  "data op" should "support finite enumerations" in {
//    val module = Module("Test_Cast", Seq(), Seq(
//      PatternFunction(None, "1_to_10", Seq(), Seq(AnnoParam(None, NatTyp)), Seq(
//        Body(Seq(Yield(DataOpCall(succOp, Seq(DataOpCall(zeroOp, Seq()).typed(NatTyp))).typed(NatTyp)))),
//        Body(
//          Seq(
//            Assign(Seq("pred"), Call("1_to_10", Seq()).typed(NatTyp)),
//            Assert(Eval(Seq("pred"), q"pred.toInt < 10").typed(TBool)),
//            Yield(DataOpCall(succOp, Seq(Var("pred").typed(NatTyp))).typed(NatTyp))
//          )
//        )
//      ))
//    ))
//
//    val input = Exp.BooleanLit(true)
//
//    assertMatchCoreProg(module, "1_to_10", input, scope) { matcher =>
//      assert(matcher.getAllMatches.size() == 10)
//    }
//  }
}
