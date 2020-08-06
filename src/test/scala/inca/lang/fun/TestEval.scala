package inca.lang.fun

import inca.IncaMatchers
import inca.analyzedLangs.Exp
import inca.lang.fun.Fun._
import inca.runtime.context.QueryScope
import org.scalatest.flatspec.AnyFlatSpec

class TestEval extends AnyFlatSpec with IncaMatchers {

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  val scope = new QueryScope(Exp.languageMetaInfo)

  "eval" can "yield a constant" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "integerlits", Seq(), Seq(AnnoParam(None, TNode(Exp.expTag))), Seq(Body(Seq(
        Assert(InstanceOf(Var("e"), TNode(Exp.expTag))),
        Assign(Seq("pi"), Eval(Map(), TInt, "Math.PI")),
        Assert(Neq(Var("pi"), Constant(DoubleLiteral(3.14)))),
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

    assertMatch(module, "integerlits", input, scope) { matcher =>
      assert(matcher.getAllMatches.size() == 8)
    }
  }

  "eval" can "be used to filter" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "integerlits", Seq(), Seq(AnnoParam(None, TNode(Exp.expTag))), Seq(Body(Seq(
        Assert(InstanceOf(Var("e"), TNode(Exp.intTag))),
        Assign(Seq("i"), PathAccess(Var("e"), NamedLink(TNode(Exp.intTag), "value")).typed(TInt)),
        Assign(Seq("cond"),
          Eval(Map("i" -> Some(TInt)), TBool,
                     s"""{ // filters square numbers
                         |  val i = env.getValue("i").asInstanceOf[Int]
                         |  Math.sqrt(i).isValidInt
                         |}""".stripMargin)),
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

    assertMatch(module, "integerlits", input, scope) { matcher =>
      assert(matcher.getAllMatches.size() == 2)
    }
  }

}
