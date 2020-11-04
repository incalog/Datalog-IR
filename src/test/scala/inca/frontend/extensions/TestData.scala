package inca.frontend.extensions

import inca.IncaMatchers
import inca.analyzedData.Nat._
import inca.analyzedLangs.Exp
import inca.compiler.Options
import inca.frontend.core._
import inca.runtime.context.QueryScope
import inca.util.Meta.Scala
import org.scalatest.flatspec.AnyFlatSpec

import scala.meta.quasiquotes._

class TestData extends AnyFlatSpec with IncaMatchers {
  val scope = new QueryScope(Exp.languageMetaInfo)
  val options = Options(scope.langMetaInfo)

  implicit def name(s: String): Name = Name(s)

  "data op" should "support zero-arg data op" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "zero", Seq(), Seq(AnnoParam(None, NatTyp)), Seq(
        Body(Seq(Yield(Eval(Seq(), zeroOp))))
      ))
    ))

    val input = Exp.BooleanLit(true)

    assertMatchCoreProg(module, "zero", input, scope) { matcher =>
      assert(matcher.getAllMatches.size() == 1)
    }
  }

  "data op" should "support non-zero-arg data op" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "one", Seq(), Seq(AnnoParam(None, NatTyp)), Seq(
        Body(Seq(Yield(EvalCall(succOp, Seq(Eval(Seq(), zeroOp))))))
      ))
    ))

    val input = Exp.BooleanLit(true)

    assertMatchCoreProg(module, "one", input, scope) { matcher =>
      assert(matcher.getAllMatches.size() == 1)
    }
  }

  "data op" should "support finite enumerations" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "1_to_10", Seq(), Seq(AnnoParam(None, NatTyp)), Seq(
        Body(Seq(Yield(EvalCall(succOp, Seq(Eval(Seq(), zeroOp)))))),
        Body(
          Seq(
            Assign(Seq("pred"), Call("1_to_10", Seq())),
            Assert(Eval(Seq(EvalParam("pred")), Scala(q"pred.toInt < 10"))),
            Yield(EvalCall(succOp, Seq(Var("pred"))))
          )
        )
      ))
    ))

    val input = Exp.BooleanLit(true)

    assertMatchCoreProg(module, "1_to_10", input, scope) { matcher =>
      assert(matcher.getAllMatches.size() == 10)
    }
  }
}
