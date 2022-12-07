package inca.frontend.constraint.core

import inca.analyzedData.Nat._
import inca.analyzedLangs.Exp
import inca.frontend.constraint.compiler.ConstraintOptions
import inca.frontend.constraint.extensions.evalCall.Trees._
import inca.frontend.datalog.Relation
import inca.runtime.context.QueryScope
import inca.util.Scala
import inca.util.matchers.IncaConstraintMatchers
import org.scalatest.flatspec.AnyFlatSpec

import scala.language.implicitConversions
import scala.meta.XtensionQuasiquoteTerm

class TestAggregate extends AnyFlatSpec with IncaConstraintMatchers {
  val scope = new QueryScope(Exp.model)
  val options = ConstraintOptions()
  val dataModel = Exp.model

  implicit def name(s: String): Name = Name(s)

  val limit = 4

  "aggregate" should "support non-invertible joins" in {
    val module = Module("Test_Cast", Seq(DirectDataModel(Exp.model)), Seq(), Seq(), Seq(
      PatternFunction(Seq(), None, "1_to_10", Seq(), TTuple(Seq(NatTyp,NatTyp)), Seq(
        Body(Seq(
          Assign(Seq("key"), Eval(Seq(), zeroOp)),
          Yield(Tuple(Seq(Var("key"), EvalCall(succOp, Seq(Eval(Seq(), zeroOp))))))
        )),
        Body(Seq(
          Assign(Seq("key"), EvalCall(succOp, Seq(Eval(Seq(), zeroOp)))),
          Yield(Tuple(Seq(Var("key"), EvalCall(succOp, Seq(Eval(Seq(), zeroOp))))))
        )),
        Body(
          Seq(
            Assign(Seq("k", "pred"), Call("1_to_10", Seq())),
            Assert(Eval(Seq(EvalParam("pred")), Scala(q"pred.toInt < $limit"))),
            Yield(Tuple(Seq(Var("k"), EvalCall(succOp, Seq(Var("pred"))))))
          )
        )
      )),

      PatternFunction(Seq(), None, "sum_1_to_10", Seq(), NatTyp, Seq(
        Body(Seq(
          Yield(Aggregate(
            Eval(Seq(), sumAggregation),
            Seq(Body(
              Assign(Seq("m", "n"), Call("1_to_10", Seq())),
              Yield(Var("n"))
            ))))
        ))
      ))
    ))

    val input = Exp.BooleanLit(true)

    assertMatch(module, "1_to_10", input) { matcher =>
      val rel = Relation.fromMatcher(matcher)
      println(rel.asTable)
      assert(matcher.getAllMatches.size() == limit * 2)
    }

    assertMatch(module, "AggregateCollection$0", input) { matcher =>
      val rel = Relation.fromMatcher(matcher)
      println(rel.asTable)
      assert(matcher.getAllMatches.size() == limit * 2)
    }

    assertMatch(module, "sum_1_to_10", input) { matcher =>
      val rel = Relation.fromMatcher(matcher)
      println(rel.asTable)
      assert(matcher.getAllMatches.size() == 1)
      assert(matcher.getAllMatchArrays.head.head.asInstanceOf[Nat].toInt == (1 to limit).sum * 2)
    }

  }

  "aggregate" should "support invertible joins" in {
    val module = Module("Test_Cast", Seq(DirectDataModel(Exp.model)), Seq(), Seq(), Seq(
      PatternFunction(Seq(), None, "1_to_10", Seq(), NatTyp, Seq(
        Body(Seq(Yield(EvalCall(succOp, Seq(Eval(Seq(), zeroOp)))))),
        Body(
          Seq(
            Assign(Seq("pred"), Call("1_to_10", Seq())),
            Assert(Eval(Seq(EvalParam("pred")), Scala(q"pred.toInt < 10"))),
            Yield(EvalCall(succOp, Seq(Var("pred"))))
          )
        )
      )),

      PatternFunction(Seq(), None, "sum_1_to_10", Seq(), NatTyp, Seq(
        Body(Seq(
          Yield(Aggregate(Eval(fastSumAggregation), Call("1_to_10", Seq()).typed(NatTyp)))
        ))
      ))
    ))

    val input = Exp.BooleanLit(true)

    assertMatch(module, "sum_1_to_10", input) { matcher =>
      assert(matcher.getAllMatches.size() == 1)
      assert(matcher.getAllMatchArrays.head.head.asInstanceOf[Nat].toInt == (1 to 10).sum)
    }
  }

  "aggregate" should "support non-invertible joins 2" in {
    val module = Module("Test_Cast", Seq(DirectDataModel(Exp.model)), Seq(), Seq(), Seq(
      ScalaModuleContent(Scala(q"import inca.analyzedData.Nat.sumAgg")),
      ScalaModuleContent(Scala(q"val nine = 9")),
      ScalaModuleContent(Scala(q"object One { val num = 1 }")),
      PatternFunction(Seq(), None, "1_to_10", Seq(), NatTyp, Seq(
        Body(Seq(Yield(EvalCall(succOp, Seq(Eval(Seq(), zeroOp)))))),
        Body(
          Seq(
            Assign(Seq("pred"), Call("1_to_10", Seq())),
            Assert(Eval(Scala(q"pred.toInt < (nine + One.num)"))),
            Yield(EvalCall(succOp, Seq(Var("pred"))))
          )
        )
      )),

      PatternFunction(Seq(), None, "sum_1_to_10", Seq(), NatTyp, Seq(
        Body(Seq(
          Yield(Aggregate(Eval(Scala(q"sumAgg")), Call("1_to_10", Seq()).typed(NatTyp)))
        ))
      ))
    ))

    val input = Exp.BooleanLit(true)

    assertMatch(module, "sum_1_to_10", input) { matcher =>
      assert(matcher.getAllMatches.size() == 1)
      assert(matcher.getAllMatchArrays.head.head.asInstanceOf[Nat].toInt == (1 to 10).sum)
    }
  }

}
