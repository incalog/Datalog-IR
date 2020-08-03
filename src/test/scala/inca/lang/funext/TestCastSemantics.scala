package inca.lang.funext

import inca.IncaMatchers
import inca.lang.fun.Fun._
import org.scalatest.flatspec.AnyFlatSpec

class TestCastSemantics extends AnyFlatSpec with IncaMatchers {

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  "desugaring" should "eliminate casts" in {
    val sugared = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "IntegerLit", Seq(), Seq(AnnoParam(None, TNode("IntegerLit"))), Seq(Body(Seq(
        Assert(InstanceOf(Var("e"), TNode("Exp"))),
        Yield(Cast(Var("e"), TNode("IntegerLit")))
      ))))
    ))

//    assertDesugar(core, sugared, Cast)
  }


}
