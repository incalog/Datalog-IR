package inca.lang.funext

import inca.lang.fun.Fun._
import org.scalatest.flatspec.AnyFlatSpec

class TestCastDesugar extends AnyFlatSpec with DesugarMatchers {

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  "desugaring" should "eliminate casts" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assert(Eq(Cast(one, TBool), Cast(two, TNode("Decl")))),
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assign(Seq("cast"), one),
        Assert(InstanceOf(Var("cast"), TBool)),
        Assign(Seq("cast0"), two),
        Assert(InstanceOf(Var("cast0"), TNode("Decl"))),
        Assert(Eq(Var("cast"), Var("cast0"))),
      ))))
    ))

    assertDesugar(core, sugared, Cast)
  }

  "desugaring" should "eliminate nested casts" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assert(Eq(Cast(Cast(one, TBool), TString), Cast(Cast(two, TNode("Decl")), TNode("Method")))),
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assign(Seq("cast"), one),
        Assert(InstanceOf(Var("cast"), TBool)),
        Assign(Seq("cast0"), Var("cast")),
        Assert(InstanceOf(Var("cast0"), TString)),
        Assign(Seq("cast1"), two),
        Assert(InstanceOf(Var("cast1"), TNode("Decl"))),
        Assign(Seq("cast2"), Var("cast1")),
        Assert(InstanceOf(Var("cast2"), TNode("Method"))),
        Assert(Eq(Var("cast0"), Var("cast2"))),
      ))))
    ))

    assertDesugar(core, sugared, Cast)
  }

}
