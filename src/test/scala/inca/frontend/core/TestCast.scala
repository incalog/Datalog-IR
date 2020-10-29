package inca.frontend.core

import inca.analyzedLangs.Exp
import inca.frontend.BaseFrontend
import inca.frontend.core.Core._
import inca.runtime.context.QueryScope
import inca.{CompilerOptions, IncaMatchers}
import org.scalatest.flatspec.AnyFlatSpec

class TestCast extends AnyFlatSpec with IncaMatchers {

  implicit def name(s: String): Name = Name(s)

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  val scope: QueryScope = new QueryScope(Exp.languageMetaInfo)
  val options: CompilerOptions = CompilerOptions(scope.langMetaInfo, new BaseFrontend(_){})
  
  "compiler" should "implement cast semantics" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "integerlits", Seq(), Seq(AnnoParam(None, TNode(Exp.expTag))), Seq(Body(Seq(
        Values("e", TNode(Exp.expTag)),
        Yield(Cast(Var("e").typed(TNode(Exp.expTag)), TNode(Exp.intTag)))
      ))))
    ), Seq())

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

    assertMatchCoreProg(module, "integerlits", input) { matcher =>
      assert(matcher.getAllMatches.size() == 5)
    }
  }


}
