package inca.frontend.extensions

import inca.IncaMatchers
import inca.analyzedLangs.Exp
import inca.compiler.Options
import inca.frontend.BaseFrontend
import inca.frontend.core._
import inca.runtime.context.QueryScope
import org.scalatest.flatspec.AnyFlatSpec

class TestIfThenElse extends AnyFlatSpec with IncaMatchers {

  implicit def name(s: String): Name = Name(s)

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))
  val three = Constant(IntLiteral(3))
  val four = Constant(IntLiteral(4))

  val scope: QueryScope = new QueryScope(Exp.languageMetaInfo)
  val options: Options = Options(scope.langMetaInfo, new BaseFrontend(_) with IfThenElseFrontend)

  "desugaring" should "eliminate if-then-else" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        IfThenElse(Eq(one, two), Body(
          Assign(Seq("yes"), Constant(BooleanLiteral(true)))
        ), Seq(), Some(Body(
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

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate nested if-then-else" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        IfThenElse(Eq(one, two), Body(
          Assign(Seq("yes"), Constant(BooleanLiteral(true))),
          IfThenElse(Eq(three, four), Body(
            Assign(Seq("yes2"), Constant(BooleanLiteral(true)))
          ), Seq(), Some(Body(
            Assign(Seq("yes2"), Constant(BooleanLiteral(false)))
          )))
        ), Seq(), Some(Body(
          Assign(Seq("yes"), Constant(BooleanLiteral(false))),
          IfThenElse(Eq(three, four), Body(
            Assign(Seq("yes2"), Constant(BooleanLiteral(true)))
          ), Seq(), Some(Body(
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

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate if-then-else-if" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        IfThenElse(Eq(one, two), Body(
          Assign(Seq("yes"), Constant(BooleanLiteral(true)))
        ), Seq(ElseIf(Eq(three, four), Body(
          Assign(Seq("yes"), Constant(IntLiteral(99)))
        ))), Some(Body(
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

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate if" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assign(Seq("before"), Constant(BooleanLiteral(true))),
        IfThenElse(Eq(one, two), Body(
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

    assertDesugar(core, sugared)
  }


  "desugaring" should "implement if-then-else semantics" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "integerlits", Seq(), Seq(AnnoParam(None, TLiteral.Int)), Seq(Body(Seq(
        Values("root", TNode(Exp.expTag)),
        Assert(Undef(PathAccess(Var("root"), ParentLink))),
        Yield(Call("integerlits_rec",Seq(Var("root"))))
      )))),

      PatternFunction(None, "integerlits_rec", Seq(Param("e", TNode(Exp.expTag))), Seq(AnnoParam(None, TLiteral.Int)), Seq(Body(Seq(
        IfThenElse(InstanceOf(Var("e"), TNode(Exp.intTag)), Body(
          Yield(PathAccess(Cast(Var("e"), TNode(Exp.intTag)), NamedLink("value")))
        ), Seq(ElseIf(InstanceOf(Var("e"), TNode(Exp.addTag)), Body(
          Yield(
            Call("integerlits_rec",
              Seq(PathAccess(Cast(Var("e"), TNode(Exp.addTag)), NamedLink("lhs")))
            )
          )
        )), ElseIf(InstanceOf(Var("e"), TNode(Exp.multTag)), Body(
          Yield(
            Call("integerlits_rec",
              Seq(PathAccess(Cast(Var("e"), TNode(Exp.multTag)), NamedLink("rhs")))
            )
          )
        ))), Some(Body(
          FailStatement
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

    assertMatchCoreProg(module, "integerlits_rec", input) { matcher =>
      assert(matcher.getAllMatches.size() == 7)
    }

    assertMatchCoreProg(module, "integerlits", input) { matcher =>
      assert(matcher.getAllMatches.size() == 1)
      matcher.getAllMatchArrays should contain theSameElementsAs Seq(Array(2))
    }
  }
}
