package inca.lang.funext

import inca.lang.fun.Fun._
import org.scalatest.flatspec.AnyFlatSpec

class TestForeachDesugar extends AnyFlatSpec with DesugarMatchers {

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  "desugaring" should "eliminate foreach loops" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Foreach("x", Var("list").typed(TList(TNode("Elem"))), Seq(
          Assert(Eq(one, Var("x"))),
          Assert(Neq(Var("x"), two))
        )),
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assign(Seq("x"), PathAccess(Var("list"), ChildrenLink)),
        Assert(Eq(one, Var("x"))),
        Assert(Neq(Var("x"), two)),
      ))))
    ))

    assertDesugar(core, sugared, Foreach)
  }

  "desugaring" should "eliminate nested foreach loops" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Foreach("x", Var("list").typed(TList(TNode("Elem"))), Seq(
          Assert(Eq(one, Var("x"))),
          Foreach("y", Var("list2").typed(TList(TNode("Elem"))), Seq(
            Assert(Eq(Var("x"), Var("y")))
          )),
          Assert(Neq(Var("x"), two))
        )),
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assign(Seq("x"), PathAccess(Var("list"), ChildrenLink)),
        Assert(Eq(one, Var("x"))),
        Assign(Seq("y"), PathAccess(Var("list2"), ChildrenLink)),
        Assert(Eq(Var("x"), Var("y"))),
        Assert(Neq(Var("x"), two)),
      ))))
    ))

    assertDesugar(core, sugared, Foreach)
  }
}
