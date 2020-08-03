package inca.lang.funext

import inca.IncaMatchers
import inca.lang.fun.Fun._
import org.scalatest.flatspec.AnyFlatSpec

class TestNotDesugar extends AnyFlatSpec with IncaMatchers {

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  "desugaring" should "eliminate not conditions" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assert(Not(Eq(one, two))),
        Assert(Not(Neq(one, two))),
        Assert(Not(InstanceOf(one, TNode("Num")))),
        Assert(Not(NotInstanceOf(one, TNode("Num")))),
        Assert(Not(Def(one))),
        Assert(Not(Undef(one))),
        Assert(Not(BooleanCond(true))),
        Assert(Not(BooleanCond(false)))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assert(Neq(one, two)),
        Assert(Eq(one, two)),
        Assert(NotInstanceOf(one, TNode("Num"))),
        Assert(InstanceOf(one, TNode("Num"))),
        Assert(Undef(one)),
        Assert(Def(one)),
        Assert(BooleanCond(false)),
        Assert(BooleanCond(true))
      ))))
    ))

    assertDesugar(core, sugared, Not)
  }

  "desugaring" should "eliminate nested not conditions" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assert(Not(Not(Eq(one, two)))),
        Assert(Not(Not(Not(Eq(one, two))))),
        Assert(Not(Not(Not(Not(Eq(one, two)))))),
        Assert(Not(Not(Not(Not(Not(Eq(one, two)))))))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assert(Eq(one, two)),
        Assert(Neq(one, two)),
        Assert(Eq(one, two)),
        Assert(Neq(one, two))
      ))))
    ))

    assertDesugar(core, sugared, Not)
  }
}
