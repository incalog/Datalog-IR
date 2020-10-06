package inca.frontend.extensions

import inca.analyzedLangs.Exp
import inca.frontend.core.Core._
import inca.runtime.context.QueryScope
import inca.{CompilerOptions, IncaMatchers}
import org.scalatest.flatspec.AnyFlatSpec

class TestEnum extends AnyFlatSpec with IncaMatchers {

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  val scope: QueryScope = new QueryScope(Exp.languageMetaInfo)
  val options: CompilerOptions = CompilerOptions(scope.langMetaInfo, desugarables = Seq(Enum))

  "desugaring" should "eliminate enum" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(
        Body(Seq(
          Assign(Seq("x"), Enum(TNode("Foo"))),
          Assign(Seq("y"), Enum(TNode("Bar"))),
          Assert(Eq(
            PathAccess(Var("x").typed(TNode("Foo")), NamedLink("name")).typed(TString),
            PathAccess(Var("y").typed(TNode("Bar")), NamedLink("name")).typed(TString))),
          Yield(Tuple(Seq(Var("x"), Var("y"))))
        ))
      ))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Values("enum_Foo", TNode("Foo")),
        Assign(Seq("x"), Var("enum_Foo")),
        Values("enum_Bar", TNode("Bar")),
        Assign(Seq("y"), Var("enum_Bar")),
        Assert(Eq(
          PathAccess(Var("x").typed(TNode("Foo")), NamedLink("name")).typed(TString),
          PathAccess(Var("y").typed(TNode("Bar")), NamedLink("name")).typed(TString))),
        Yield(Tuple(Seq(Var("x"), Var("y"))))
      ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate foreach enum" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(AnnoParam(None, TNode("Method"))), Seq(
        Body(Seq(
          Foreach("meth", Enum(TNode("Method")).typed(TEnumeration(TNode("Method"))), Body(
            IfThenElse(
              Eq(
                PathAccess(Var("meth").typed(TNode("Method")), NamedLink("name")).typed(TString),
                Constant(StringLiteral("main"))),
              Body(Yield(Var("meth"))),
              Seq(),
              None),
            Fail
          ))
        ))
      ))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(AnnoParam(None, TNode("Method"))), Seq(
        Body(Seq(
          Values("enum_Method", TNode("Method")),
          Assign(Seq("meth"), Var("enum_Method")),
          Assert(Eq(
            PathAccess(Var("meth").typed(TNode("Method")), NamedLink("name")).typed(TString),
            Constant(StringLiteral("main")))),
          Yield(Var("meth")),
          Fail
        )),
        Body(Seq(
          Values("enum_Method", TNode("Method")),
          Assign(Seq("meth"), Var("enum_Method")),
          Fail
        ))
      ))
    ))

    assertDesugar(core, sugared, CompilerOptions(scope.langMetaInfo, desugarables = Seq(Enum, Foreach, IfThenElse)))
  }


  "desugaring" should "implement enum semantics" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "integerlits", Seq(), Seq(AnnoParam(None, TNode(Exp.expTag))), Seq(Body(Seq(
        Assign(Seq("i"), Enum(TNode(Exp.intTag))),
        Yield(Var("i"))
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

    assertMatchCoreProg(module, "integerlits", input) { matcher =>
      assert(matcher.getAllMatches.size() == 5)
    }
  }
}
