package inca.frontend.funext

import inca.IncaMatchers
import inca.analyzedLangs.Exp
import inca.frontend.fun.Fun._
import inca.runtime.context.QueryScope
import org.scalatest.flatspec.AnyFlatSpec

class TestForallExists extends AnyFlatSpec with IncaMatchers {

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  "desugaring" should "eliminate forall conds" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(Param("many", Some(TNode("Many")))), Seq(), Seq(Body(Seq(
        Forall("x", PathAccess(Var("many").typed(TNode("Many")), NamedLink(TNode("Many"), "exps")).typed(TList(TNode("Exp"))), Body(
          Assert(Eq(one, Var("x")))
        )),
        Yield(Constant(UnitLiteral))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "forallCond", Seq(Param("many", Some(TNode("Many")))), Seq(AnnoParam(None, TNode("Exp"))), Seq(Body(Seq(
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
        Assign(Seq("successSize"), Call("forallCond", Seq(Var("many")), transitive = false, count = true)),
        Assert(Eq(Var("listSize"), Var("successSize"))),
        Yield(Constant(UnitLiteral))
      ))))
    ))

    assertDesugar(core, sugared, ForallExists)
  }

  val scope = new QueryScope(Exp.languageMetaInfo)


  "desugaring" should "implement forall list semantics" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "intLists", Seq(Param("l", Some(TList(TNode(Exp.expTag))))), Seq(), Seq(Body(Seq(
        Forall("e", Var("l").typed(TList(TNode(Exp.expTag))), Body(
          Assert(InstanceOf(Var("e"), TNode(Exp.intTag)))
        )),
        Yield(Constant(UnitLiteral))
      ))))
    ))

    val input = {
      import Exp._
      Add(
        Mul(
          IntegerLit(1),
          IntegerLit(2)
        ),
        Many(
          List(
            IntegerLit(3),
            IntegerLit(4),
            IntegerLit(5)
          )
        )
      )
    }

    assertMatch(module, "forallCond", input, scope, ForallExists) { matcher =>
      assert(matcher.getAllMatchArrays.size == 3)
    }

    assertMatch(module, "intLists", input, scope, ForallExists) { matcher =>
      assert(matcher.getAllMatchArrays.size == 1)
    }
  }

  "desugaring" should "implement exists list semantics" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "listContaining4", Seq(Param("l", Some(TList(TNode(Exp.expTag))))), Seq(), Seq(Body(Seq(
        Exists("e", Var("l").typed(TList(TNode(Exp.expTag))), Body(
          Assign(Seq("i"), PathAccess(Cast(Var("e").typed(TNode(Exp.expTag)), TNode(Exp.intTag)), NamedLink(TNode(Exp.intTag), "value")).typed(TInt)),
          Assert(Eval(Seq("i"), TBool, """i == 4"""))
        )),
        Yield(Constant(UnitLiteral))
      ))))
    ))

    val input = {
      import Exp._
      Add(
        Mul(
          IntegerLit(1),
          IntegerLit(2)
        ),
        Many(
          List(
            IntegerLit(3),
            IntegerLit(4),
            IntegerLit(5)
          )
        )
      )
    }

    assertMatch(module, "existsCond", input, scope, ForallExists, Cast) { matcher =>
      assert(matcher.getAllMatchArrays.size == 1)
    }

    assertMatch(module, "listContaining4", input, scope, ForallExists, Cast) { matcher =>
      assert(matcher.getAllMatchArrays.size == 1)
    }
  }
}
