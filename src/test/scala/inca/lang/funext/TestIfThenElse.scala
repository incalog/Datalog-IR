package inca.lang.funext

import inca.IncaMatchers
import inca.analyzedLangs.Exp
import inca.lang.fun.Fun._
import inca.runtime.context.QueryScope
import org.scalatest.flatspec.AnyFlatSpec

class TestIfThenElse extends AnyFlatSpec with IncaMatchers {

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
        ))),
        Assign(Seq("after"), Constant(BooleanLiteral(true)))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assert(Eq(one, two)),
        Assign(Seq("yes"), Constant(BooleanLiteral(true))),
        Assign(Seq("after"), Constant(BooleanLiteral(true)))
      )),
      Body(Seq(
        Assert(Neq(one, two)),
        Assign(Seq("yes"), Constant(BooleanLiteral(false))),
        Assign(Seq("after"), Constant(BooleanLiteral(true)))
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
        ))),
        Assign(Seq("after"), Constant(BooleanLiteral(true)))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assert(Eq(one, two)),
        Assign(Seq("yes"), Constant(BooleanLiteral(true))),
        Assert(Eq(three, four)),
        Assign(Seq("yes2"), Constant(BooleanLiteral(true))),
        Assign(Seq("after"), Constant(BooleanLiteral(true)))
      )),
      Body(Seq(
        Assert(Eq(one, two)),
        Assign(Seq("yes"), Constant(BooleanLiteral(true))),
        Assert(Neq(three, four)),
        Assign(Seq("yes2"), Constant(BooleanLiteral(false))),
        Assign(Seq("after"), Constant(BooleanLiteral(true)))
      )),
      Body(Seq(
        Assert(Neq(one, two)),
        Assign(Seq("yes"), Constant(BooleanLiteral(false))),
        Assert(Eq(three, four)),
        Assign(Seq("yes2"), Constant(BooleanLiteral(true))),
        Assign(Seq("after"), Constant(BooleanLiteral(true)))
      )),
      Body(Seq(
        Assert(Neq(one, two)),
        Assign(Seq("yes"), Constant(BooleanLiteral(false))),
        Assert(Neq(three, four)),
        Assign(Seq("yes2"), Constant(BooleanLiteral(false))),
        Assign(Seq("after"), Constant(BooleanLiteral(true)))
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
        ))),
        Assign(Seq("after"), Constant(BooleanLiteral(true)))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(
        Body(Seq(
          Assert(Eq(one, two)),
          Assign(Seq("yes"), Constant(BooleanLiteral(true))),
          Assign(Seq("after"), Constant(BooleanLiteral(true)))
        )),
        Body(Seq(
          Assert(Neq(one, two)),
          Assert(Eq(three, four)),
          Assign(Seq("yes"), Constant(IntLiteral(99))),
          Assign(Seq("after"), Constant(BooleanLiteral(true)))
        )),
        Body(Seq(
          Assert(Neq(one, two)),
          Assert(Neq(three, four)),
          Assign(Seq("yes"), Constant(BooleanLiteral(false))),
          Assign(Seq("after"), Constant(BooleanLiteral(true)))
        ))))
    ))

    assertDesugar(core, sugared, IfThenElse)
  }

  "desugaring" should "eliminate if" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assign(Seq("before"), Constant(BooleanLiteral(true))),
        IfThenElse(Eq(one, two), Seq(
          Assign(Seq("yes"), Constant(BooleanLiteral(true)))
        ), Seq(), None),
        Assign(Seq("after"), Constant(BooleanLiteral(true)))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assign(Seq("before"), Constant(BooleanLiteral(true))),
        Assert(Eq(one, two)),
        Assign(Seq("yes"), Constant(BooleanLiteral(true))),
        Assign(Seq("after"), Constant(BooleanLiteral(true)))
      )),
        Body(Seq(
          Assign(Seq("before"), Constant(BooleanLiteral(true))),
          Assign(Seq("after"), Constant(BooleanLiteral(true)))
        ))))
    ))

    assertDesugar(core, sugared, IfThenElse)
  }


  val scope = new QueryScope(Exp.languageMetaInfo)

  "desugaring" should "implement if-then-else semantics" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "integerlits", Seq(), Seq(AnnoParam(None, TInt)), Seq(Body(Seq(
        Values("root", TNode(Exp.expTag)),
        Assert(Undef(PathAccess(Var("root").typed(TNode(Exp.expTag)), ParentLink).typed(TAnyLinked))),
        Yield(
          Call("integerlits_rec",
            Seq(Var("root")),
            transitive = false, count = false
          )
        )
      )))),

      PatternFunction(None, "integerlits_rec", Seq(Param("e", Some(TNode(Exp.expTag)))), Seq(AnnoParam(None, TInt)), Seq(Body(Seq(
        IfThenElse(InstanceOf(Var("e"), TNode(Exp.intTag)), Seq(
          Yield(PathAccess(Var("e"), NamedLink(TNode(Exp.intTag), "value")).typed(TInt))
        ), Seq(ElseIf(InstanceOf(Var("e"), TNode(Exp.addTag)), Seq(
          Yield(
            Call("integerlits_rec",
              Seq(PathAccess(Var("e"), NamedLink(TNode(Exp.addTag), "lhs")).typed(TNode(Exp.expTag))),
              transitive = false, count = false
            )
          )
        )), ElseIf(InstanceOf(Var("e"), TNode(Exp.multTag)), Seq(
          Yield(
            Call("integerlits_rec",
              Seq(PathAccess(Var("e"), NamedLink(TNode(Exp.multTag), "rhs")).typed(TNode(Exp.expTag))),
              transitive = false, count = false))
        ))), Some(Seq(
          Fail
        ))),
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

    assertMatch(module, "generated_helper_undefpath_ParentLink", input, scope, IfThenElse) { matcher =>
      assert(matcher.getAllMatches.size() == 7)
    }

    assertMatch(module, "integerlits_rec", input, scope, IfThenElse) { matcher =>
      assert(matcher.getAllMatches.size() == 7)
    }

    assertMatch(module, "integerlits", input, scope, IfThenElse) { matcher =>
      assert(matcher.getAllMatches.size() == 1)
      matcher.getAllMatchArrays should contain theSameElementsAs Seq(Array(2))
    }
  }
}
