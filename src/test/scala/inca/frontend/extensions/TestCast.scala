package inca.frontend.extensions

import inca.analyzedLangs.Exp
import inca.frontend.core.Core._
import inca.runtime.context.QueryScope
import inca.{CompilerOptions, IncaMatchers}
import org.scalatest.flatspec.AnyFlatSpec

class TestCast extends AnyFlatSpec with IncaMatchers {

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  val scope: QueryScope = new QueryScope(Exp.languageMetaInfo)
  val options: CompilerOptions = CompilerOptions(scope.langMetaInfo, desugarables = Seq(Cast))
  
  "desugaring" should "eliminate casts" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assert(Eq(Cast(one, TBool), Cast(two, TNode("Decl")))),
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assign(Seq("cast"), one),
        Assert(InstanceOf(Var("cast"), TBool)),
        Assign(Seq("cast_0"), two),
        Assert(InstanceOf(Var("cast_0"), TNode("Decl"))),
        Assert(Eq(Var("cast"), Var("cast_0"))),
      ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate nested casts" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assert(Eq(Cast(Cast(one, TBool), TString), Cast(Cast(two, TNode("Decl")), TNode("Method")))),
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assign(Seq("cast"), one),
        Assert(InstanceOf(Var("cast"), TBool)),
        Assert(InstanceOf(Var("cast"), TString)),
        Assign(Seq("cast_0"), two),
        Assert(InstanceOf(Var("cast_0"), TNode("Decl"))),
        Assert(InstanceOf(Var("cast_0"), TNode("Method"))),
        Assert(Eq(Var("cast"), Var("cast_0"))),
      ))))
    ))

    assertDesugar(core, sugared)
  }


  "desugaring" should "implement cast semantics" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "integerlits", Seq(), Seq(AnnoParam(None, TNode(Exp.expTag))), Seq(Body(Seq(
        Values("e", TNode(Exp.expTag)),
        Yield(Cast(Var("e").typed(TNode(Exp.expTag)), TNode(Exp.intTag)))
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
      assert(matcher.getAllMatches.size() == 5)
    }
  }


}
