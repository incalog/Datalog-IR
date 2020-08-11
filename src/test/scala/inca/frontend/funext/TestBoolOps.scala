package inca.frontend.funext

import inca.IncaMatchers
import inca.analyzedLangs.Exp
import inca.frontend.fun.Fun._
import inca.runtime.context.QueryScope
import org.scalatest.flatspec.AnyFlatSpec

class TestBoolOps extends AnyFlatSpec with IncaMatchers {

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
        Assert(Not(Constant(BooleanLiteral(true)))),
        Assert(Not(Constant(BooleanLiteral(false))))
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
        Assert(Constant(BooleanLiteral(false))),
        Assert(Constant(BooleanLiteral(true)))
      ))))
    ))

    assertDesugar(core, sugared, BoolOps)
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

    assertDesugar(core, sugared, BoolOps)
  }

  val scope = new QueryScope(Exp.languageMetaInfo)

  "desugaring" should "negate eval code" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "integerlits", Seq(), Seq(AnnoParam(None, TNode(Exp.expTag))), Seq(Body(Seq(
        Values("e", TNode(Exp.intTag)),
        Assign(Seq("i"), PathAccess(Var("e"), NamedLink(TNode(Exp.intTag), "value")).typed(TInt)),
        Assign(Seq("cond"),
          // "i" is _not_ a square number
          Not(Eval(Seq("i"), TBool, s"Math.sqrt(i).isValidInt"))),
        Assert(Eq(Var("cond"), Constant(BooleanLiteral(true)))),
        Yield(Var("e"))
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

    assertMatch(module, "integerlits", input, scope, BoolOps) { matcher =>
      assert(matcher.getAllMatches.size() == 3)
    }
  }

  "desugaring" should "implement `and` semantics" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "add_mul", Seq(), Seq(AnnoParam(None, TNode(Exp.expTag))), Seq(Body(Seq(
        Values("e", TNode(Exp.expTag)),
        Assert(And(
          InstanceOf(Var("e"), TNode(Exp.addTag)),
          InstanceOf(PathAccess(Var("e"), NamedLink(TNode(Exp.addTag), "lhs")).typed(TNode(Exp.expTag)), TNode(Exp.multTag))
        )),
        Yield(Var("e"))
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

    assertMatch(module, "add_mul", input, scope, BoolOps) { matcher =>
      assert(matcher.getAllMatches.size() == 1)
    }
  }

  "desugaring" should "implement `not and` semantics" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "integerLits", Seq(), Seq(AnnoParam(None, TNode(Exp.expTag))), Seq(Body(Seq(
        Values("e", TNode(Exp.expTag)),
        Assert(Not(And(
          InstanceOf(Var("e"), TNode(Exp.addTag)),
          InstanceOf(Var("e"), TNode(Exp.multTag))
        ))),
        Yield(Var("e"))
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

    assertMatch(module, "integerLits", input, scope, BoolOps) { matcher =>
      assert(matcher.getAllMatches.size() == 8)
    }
  }

  "desugaring" should "implement `not or` semantics" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "integerLits", Seq(), Seq(AnnoParam(None, TNode(Exp.expTag))), Seq(Body(Seq(
        Values("e", TNode(Exp.expTag)),
        Assert(Not(Or(
          InstanceOf(Var("e"), TNode(Exp.addTag)),
          InstanceOf(Var("e"), TNode(Exp.multTag))
        ))),
        Yield(Var("e"))
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

    assertMatch(module, "integerLits", input, scope, BoolOps) { matcher =>
      assert(matcher.getAllMatches.size() == 6)
    }
  }
}
