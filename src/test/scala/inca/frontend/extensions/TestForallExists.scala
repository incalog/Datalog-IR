package inca.frontend.extensions

import inca.analyzedLangs.Exp
import inca.frontend.core.Core._
import inca.runtime.context.QueryScope
import inca.{CompilerOptions, IncaMatchers}
import org.scalatest.flatspec.AnyFlatSpec

class TestForallExists extends AnyFlatSpec with IncaMatchers {

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  val scope: QueryScope = new QueryScope(Exp.languageMetaInfo)
  val options: CompilerOptions = CompilerOptions(scope.langMetaInfo, desugarables = Seq(ForallExists))

  "desugaring" should "eliminate forall conds" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(Param("many", TNode("Many"))), Seq(), Seq(Body(Seq(
        Forall("x", PathAccess(Var("many").typed(TNode("Many")), NamedLink(TNode("Many"), "exps")).typed(TList(TNode("Exp"))), Body(
          Assert(Eq(one, Var("x")))
        )),
        Yield(Constant(UnitLiteral))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "forallCond", Seq(Param("many", TNode("Many"))), Seq(AnnoParam(None, TNode("Exp"))), Seq(Body(Seq(
        Assign(Seq("x"),
          PathAccess(
            PathAccess(Var("many"), NamedLink(TNode("Many"), "exps")).typed(TList(TNode("Exp"))),
            ChildrenLink
          ).typed(TNode("Exp"))),
        Assert(Eq(one, Var("x"))),
        Yield(Var("x"))
      )))),
      PatternFunction(None, "foo", Seq(Param("many", TNode("Many"))), Seq(), Seq(Body(Seq(
        Assign(Seq("listSize"), PathAccess(PathAccess(Var("many"), NamedLink(TNode("Many"), "exps")).typed(TList(TNode("Exp"))), SizeLink).typed(TInt)),
        Assign(Seq("successSize"), Count(Call("forallCond", Seq(Var("many"))))),
        Assert(Eq(Var("listSize"), Var("successSize"))),
        Yield(Constant(UnitLiteral))
      ))))
    ))

    assertDesugar(core, sugared)
  }


  "desugaring" should "implement forall list semantics" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "intLists", Seq(Param("l", TList(TNode(Exp.expTag)))), Seq(), Seq(Body(Seq(
        Forall("e", Var("l").typed(TList(TNode(Exp.expTag))), Body(
          Assert(InstanceOf(Var("e"), TNode(Exp.intTag)))
        )),
        Yield(Constant(UnitLiteral))
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

    assertMatchCoreProg(module, "forallCond", input) { matcher =>
      assert(matcher.getAllMatchArrays.size == 3)
    }

    assertMatchCoreProg(module, "intLists", input) { matcher =>
      assert(matcher.getAllMatchArrays.size == 1)
    }
  }

  "desugaring" should "implement exists list semantics" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "listContaining4", Seq(Param("l", TList(TNode(Exp.expTag)))), Seq(), Seq(Body(Seq(
        Exists("e", Var("l").typed(TList(TNode(Exp.expTag))), Body(
          Assign(Seq("i"), PathAccess(Cast(Var("e").typed(TNode(Exp.expTag)), TNode(Exp.intTag)), NamedLink(TNode(Exp.intTag), "value")).typed(TInt)),
          Assert(Eval(Seq("i"), TBool, """i == 4"""))
        )),
        Yield(Constant(UnitLiteral))
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

    val options = CompilerOptions(scope.langMetaInfo, desugarables = Seq(ForallExists, Cast))

    assertMatchCoreProg(module, "existsCond", input, options = options) { matcher =>
      assert(matcher.getAllMatchArrays.size == 1)
    }

    assertMatchCoreProg(module, "listContaining4", input, options = options) { matcher =>
      assert(matcher.getAllMatchArrays.size == 1)
    }
  }
}
