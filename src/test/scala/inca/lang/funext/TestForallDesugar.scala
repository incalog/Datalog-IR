package inca.lang.funext

import inca.IncaMatchers
import inca.lang.fun.Fun._
import org.scalatest.flatspec.AnyFlatSpec

class TestForallDesugar extends AnyFlatSpec with IncaMatchers {

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  "desugaring" should "eliminate forall conds" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(Param("many", Some(TNode("Many")))), Seq(), Seq(Body(Seq(
        Forall("x", PathAccess(Var("many").typed(TNode("Many")), NamedLink(TNode("Many"), "exps")).typed(TList(TNode("Exp"))), Seq(
          Assert(Eq(one, Var("x")))
        )),
        Yield(Constant(UnitLiteral))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "forallFun", Seq(Param("many", Some(TNode("Many")))), Seq(AnnoParam(None, TNode("Exp"))), Seq(Body(Seq(
        Assign(Seq("x"),
          PathAccess(
            PathAccess(Var("many"), NamedLink(TNode("Many"), "exps")).typed(TList(TNode("Exp"))),
            ChildrenLink
          ).typed(TNode("Exp"))),
        Assert(Eq(one, Var("x"))),
        Yield(Var("x"))
      )))),
      PatternFunction(None, "foo", Seq(Param("many", Some(TNode("Many")))), Seq(), Seq(Body(Seq(
        Assign(Seq("listSize"), PathAccess(PathAccess(Var("many"), NamedLink(TNode("Many"), "exps")).typed(TList(TNode("Exp"))), SizeLink).typed(TInt)),
        Assign(Seq("successSize"), Call("forallFun", Seq(Var("many")), transitive = false, count = true)),
        Assert(Eq(Var("listSize"), Var("successSize"))),
        Yield(Constant(UnitLiteral))
      ))))
    ))

    assertDesugar(core, sugared, Forall)
  }

}
