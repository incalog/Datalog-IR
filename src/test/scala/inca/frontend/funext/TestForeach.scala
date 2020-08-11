package inca.frontend.funext

import inca.IncaMatchers
import inca.analyzedLangs.Exp
import inca.frontend.fun.Fun._
import inca.runtime.context.QueryScope
import org.scalatest.flatspec.AnyFlatSpec

class TestForeach extends AnyFlatSpec with IncaMatchers {

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  "desugaring" should "eliminate foreach loops" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Foreach("x", Var("list").typed(TList(TNode("Elem"))), Body(
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
        Foreach("x", Var("list").typed(TList(TNode("Elem"))), Body(
          Assert(Eq(one, Var("x"))),
          Foreach("y", Var("list2").typed(TList(TNode("Elem"))), Body(
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


  val scope = new QueryScope(Exp.languageMetaInfo)

  "desugaring" should "implement foreach enum semantics" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "integerlits", Seq(), Seq(AnnoParam(None, TNode(Exp.expTag))), Seq(Body(Seq(
        Foreach("i", Enum(TNode(Exp.intTag)).typed(TEnumeration(TNode(Exp.intTag))), Body(
          Yield(Var("i"))
        ))
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

    assertMatch(module, "integerlits", input, scope, Foreach, Enum) { matcher =>
      assert(matcher.getAllMatches.size() == 5)
    }
  }

  "desugaring" should "implement foreach list semantics" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "integerlits", Seq(), Seq(AnnoParam(None, TInt)), Seq(Body(Seq(
        Values("many", TNode(Exp.manyTag)),
        Foreach("i", PathAccess(Var("many"), NamedLink(TNode(Exp.manyTag), "exps")).typed(TList(TNode(Exp.intTag))), Body(
          Yield(PathAccess(Var("i"), NamedLink(TNode(Exp.intTag), "value")).typed(TInt))
        ))
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

    assertMatch(module, "integerlits", input, scope, Foreach, Enum) { matcher =>
      assert(matcher.getAllMatches.size() == 3)
      matcher.getAllMatchArrays should contain theSameElementsAs Seq(Array(3), Array(4), Array(5))
    }
  }
}
