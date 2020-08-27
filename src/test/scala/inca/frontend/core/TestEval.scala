package inca.frontend.core

import inca.analyzedLangs.Exp
import inca.frontend.core.Core._
import inca.runtime.context.QueryScope
import inca.{CompilerOptions, IncaMatchers}
import org.scalatest.flatspec.AnyFlatSpec

class TestEval extends AnyFlatSpec with IncaMatchers {

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  val scope = new QueryScope(Exp.languageMetaInfo)
  val options = CompilerOptions(scope.langMetaInfo)

  "eval" can "yield a constant" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "integerlits", Seq(), Seq(AnnoParam(None, TNode(Exp.expTag))), Seq(Body(Seq(
        Values("e", TNode(Exp.expTag)),
        Assign(Seq("pi"), Eval(Seq(), TDouble, "Math.PI")),
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
        Values("e", TNode(Exp.intTag)),
        Assign(Seq("i"), PathAccess(Var("e"), NamedLink(TNode(Exp.intTag), "value")).typed(TInt)),
        Assign(Seq("cond"),
          Eval(Seq("i"), TBool, "Math.sqrt(i).isValidInt")),
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
