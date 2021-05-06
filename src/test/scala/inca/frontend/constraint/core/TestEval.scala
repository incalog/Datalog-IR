package inca.frontend.constraint.core

import inca.analyzedLangs.Exp
import inca.compiler.options.ConstraintOptions
import inca.runtime.context.QueryScope
import inca.util.Meta.Scala
import inca.util.matchers.IncaConstraintMatchers
import org.scalatest.flatspec.AnyFlatSpec

import scala.language.implicitConversions
import scala.meta.XtensionQuasiquoteTerm

class TestEval extends AnyFlatSpec with IncaConstraintMatchers {

  implicit def name(s: String): Name = Name(s)

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  val dataModel = Exp.model
  val scope = new QueryScope(Exp.model)
  val options = ConstraintOptions()

  "eval" can "yield a constant" in {
    val module = Module("Test_Cast", Seq(DirectDataModel(Exp.model)), Seq(), Seq(), Seq(
      PatternFunction(None, "integerlits", Seq(), TNode(Exp.expTag), Seq(Body(Seq(
        Values("e", TNode(Exp.expTag)),
        Assign(Seq("pi"), Eval(Seq(), Scala(q"Math.PI"))),
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

    assertMatch(module, "integerlits", input) { matcher =>
      assert(matcher.getAllMatches.size() == 8)
    }
  }

  "eval" can "be used to filter" in {
    val module = Module("Test_Cast", Seq(DirectDataModel(Exp.model)), Seq(), Seq(), Seq(
      PatternFunction(None, "integerlits", Seq(), TNode(Exp.expTag), Seq(Body(Seq(
        Values("e", TNode(Exp.intTag)),
        Assign(Seq("i"), PathAccess(Var("e"), NamedLink("value"))),
        Assign(Seq("cond"),
          Eval(Seq(EvalParam("i")), Scala(q"Math.sqrt(i.doubleValue()).isValidInt"))),
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

    assertMatch(module, "integerlits", input) { matcher =>
      assert(matcher.getAllMatches.size() == 2)
    }
  }

}
