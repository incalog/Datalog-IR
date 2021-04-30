package inca.frontend.core

import inca.IncaMatchers
import inca.analyzedLangs.Exp
import inca.compiler.Options
import inca.frontend.core.tree._
import inca.runtime.context.QueryScope
import org.scalatest.flatspec.AnyFlatSpec

import scala.language.implicitConversions

class TestCast extends AnyFlatSpec with IncaMatchers {

  implicit def name(s: String): Name = Name(s)

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  val dataModel = Exp.model
  val scope: QueryScope = new QueryScope(Exp.model)
  val options: Options = Options()
  
  "compiler" should "implement cast semantics" in {
    val module = Module("Test_Cast", Seq(DirectDataModel(Exp.model)), Seq(), Seq(), Seq(
      PatternFunction(None, "integerlits", Seq(), TNode(Exp.expTag), Seq(Body(Seq(
        Values("e", TNode(Exp.expTag)),
        Yield(Cast(Var("e"), TNode(Exp.intTag)))
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

    assertMatchFunModule(module, "integerlits", input) { matcher =>
      assert(matcher.getAllMatches.size() == 5)
    }
  }


}
