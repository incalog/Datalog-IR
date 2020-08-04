package inca.lang.funext

import inca.IncaMatchers
import inca.analyzedLangs.Exp
import inca.lang.fun.Fun._
import inca.runtime.context.QueryScope
import org.scalatest.flatspec.AnyFlatSpec

class TestEnum extends AnyFlatSpec with IncaMatchers {

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  "desugaring" should "eliminate enum" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(
        Body(Seq(
          Assign(Seq("x"), Enum(TNode("Foo"))),
          Assign(Seq("y"), Enum(TNode("Bar"))),
          Assert(Eq(
            PathAccess(Var("x"), NamedLink(TNode("Foo"), "name")).typed(TString),
            PathAccess(Var("y"), NamedLink(TNode("Bar"), "name")).typed(TString))),
          Yield(Tuple(Seq(Var("x"), Var("y"))))
        ))
      ))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Assert(InstanceOf(Var("enum_Foo"), TNode("Foo"))),
        Assign(Seq("x"), Var("enum_Foo")),
        Assert(InstanceOf(Var("enum_Bar"), TNode("Bar"))),
        Assign(Seq("y"), Var("enum_Bar")),
        Assert(Eq(
          PathAccess(Var("x"), NamedLink(TNode("Foo"), "name")).typed(TString),
          PathAccess(Var("y"), NamedLink(TNode("Bar"), "name")).typed(TString))),
        Yield(Tuple(Seq(Var("x"), Var("y"))))
      ))))
    ))

    assertDesugar(core, sugared, Enum)
  }

  "desugaring" should "eliminate foreach enum" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(AnnoParam(None, TNode("Method"))), Seq(
        Body(Seq(
          Foreach("meth", Enum(TNode("Method")).typed(TEnumeration(TNode("Method"))), Seq(
            IfThenElse(
              Eq(
                PathAccess(Var("meth"), NamedLink(TNode("Method"), "name")).typed(TString),
                Constant(StringLiteral("main"))),
              Seq(Yield(Var("meth"))),
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
          Assert(InstanceOf(Var("enum_Method"), TNode("Method"))),
          Assign(Seq("meth"), Var("enum_Method")),
          Assert(Eq(
            PathAccess(Var("meth"), NamedLink(TNode("Method"), "name")).typed(TString),
            Constant(StringLiteral("main")))),
          Yield(Var("meth")),
          Fail
        )),
        Body(Seq(
          Assert(InstanceOf(Var("enum_Method"), TNode("Method"))),
          Assign(Seq("meth"), Var("enum_Method")),
          Fail
        ))
      ))
    ))

    assertDesugar(core, sugared, Enum, Foreach, IfThenElse)
  }


  val scope = new QueryScope(Exp.languageMetaInfo)

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

    assertMatch(module, "integerlits", input, scope, Enum) { matcher =>
      assert(matcher.getAllMatches.size() == 5)
    }
  }
}
