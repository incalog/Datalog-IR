package inca.lang.funext

import inca.IncaMatchers
import inca.analyzedLangs.Exp
import inca.lang.fun.Fun._
import inca.runtime.context.QueryScope
import org.scalatest.flatspec.AnyFlatSpec

class TestNot extends AnyFlatSpec with IncaMatchers {

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

  val scope = new QueryScope(Exp.languageMetaInfo)
  "eval" can "be used to filter" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "integerlits", Seq(), Seq(AnnoParam(None, TNode(Exp.expTag))), Seq(Body(Seq(
        Assert(InstanceOf(Var("e"), TNode(Exp.intTag))),
        Assign(Seq("i"), PathAccess(Var("e"), NamedLink(TNode(Exp.intTag), "value")).typed(TInt)),
        Assign(Seq("cond"),
          // "i" is _not_ a square number
          Not(Eval(Map("i" -> Some(TInt)), TBool,
            s"""{ // filters square numbers
               |  val i = env.getValue("i").asInstanceOf[Int]
               |  Math.sqrt(i).isValidInt
               |}""".stripMargin))),
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

    assertMatch(module, "integerlits", input, scope, Not) { matcher =>
      assert(matcher.getAllMatches.size() == 3)
    }
  }
}
