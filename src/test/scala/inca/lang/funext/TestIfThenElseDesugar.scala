package inca.lang.funext

import inca.lang.fun.Fun._
import org.scalatest.flatspec.AnyFlatSpec

class TestIfThenElseDesugar extends AnyFlatSpec with DesugarMatchers {

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))
  val three = Constant(IntLiteral(3))
  val four = Constant(IntLiteral(4))

  "desugaring" should "eliminate if-then-else" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        IfThenElse(Eq(one, two), Seq(
          Assign(Seq("yes"), Constant(BooleanLiteral(true)))
        ), Seq(), Some(Seq(
          Assign(Seq("yes"), Constant(BooleanLiteral(false)))
        )))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assert(Eq(one, two)),
        Assign(Seq("yes"), Constant(BooleanLiteral(true)))
      )),
      Body(Seq(
        Assert(Neq(one, two)),
        Assign(Seq("yes"), Constant(BooleanLiteral(false)))
      ))))
    ))

    assertDesugar(core, sugared, IfThenElse)
  }

  "desugaring" should "eliminate nested if-then-else" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        IfThenElse(Eq(one, two), Seq(
          Assign(Seq("yes"), Constant(BooleanLiteral(true))),
          IfThenElse(Eq(three, four), Seq(
            Assign(Seq("yes2"), Constant(BooleanLiteral(true)))
          ), Seq(), Some(Seq(
            Assign(Seq("yes2"), Constant(BooleanLiteral(false)))
          )))
        ), Seq(), Some(Seq(
          Assign(Seq("yes"), Constant(BooleanLiteral(false))),
          IfThenElse(Eq(three, four), Seq(
            Assign(Seq("yes2"), Constant(BooleanLiteral(true)))
          ), Seq(), Some(Seq(
            Assign(Seq("yes2"), Constant(BooleanLiteral(false)))
          )))
        )))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assert(Eq(one, two)),
        Assign(Seq("yes"), Constant(BooleanLiteral(true))),
        Assert(Eq(three, four)),
        Assign(Seq("yes2"), Constant(BooleanLiteral(true)))
      )),
      Body(Seq(
        Assert(Eq(one, two)),
        Assign(Seq("yes"), Constant(BooleanLiteral(true))),
        Assert(Neq(three, four)),
        Assign(Seq("yes2"), Constant(BooleanLiteral(false)))
      )),
      Body(Seq(
        Assert(Neq(one, two)),
        Assign(Seq("yes"), Constant(BooleanLiteral(false))),
        Assert(Eq(three, four)),
        Assign(Seq("yes2"), Constant(BooleanLiteral(true)))
      )),
      Body(Seq(
        Assert(Neq(one, two)),
        Assign(Seq("yes"), Constant(BooleanLiteral(false))),
        Assert(Neq(three, four)),
        Assign(Seq("yes2"), Constant(BooleanLiteral(false)))
      ))))
    ))

    assertDesugar(core, sugared, IfThenElse)
  }

  "desugaring" should "eliminate if-then-else-if" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        IfThenElse(Eq(one, two), Seq(
          Assign(Seq("yes"), Constant(BooleanLiteral(true)))
        ), Seq(ElseIf(Eq(three, four), Seq(
          Assign(Seq("yes"), Constant(IntLiteral(99)))
        ))), Some(Seq(
          Assign(Seq("yes"), Constant(BooleanLiteral(false)))
        )))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assert(Eq(one, two)),
        Assign(Seq("yes"), Constant(BooleanLiteral(true)))
      )),
      Body(Seq(
        Assert(Neq(one, two)),
        Assert(Eq(three, four)),
        Assign(Seq("yes"), Constant(IntLiteral(99)))
      )),
      Body(Seq(
        Assert(Neq(one, two)),
        Assert(Neq(three, four)),
        Assign(Seq("yes"), Constant(BooleanLiteral(false)))
      ))))
    ))

    assertDesugar(core, sugared, IfThenElse)
  }
}
