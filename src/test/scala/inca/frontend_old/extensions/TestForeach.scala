package inca.frontend_old.extensions

import inca.IncaMatchers
import inca.analyzedLangs.Exp
import inca.compiler.Options
import inca.frontend_old.core
import inca.frontend_old.core.tree._
import inca.frontend_old.extensions.foreach.Trees._
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import org.scalatest.flatspec.AnyFlatSpec

import scala.language.implicitConversions

class TestForeach extends AnyFlatSpec with IncaMatchers {

  implicit def name(s: String): Name = Name(s)

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  val scope: QueryScope = new QueryScope(Exp.languageMetaInfo)
  val options: Options = Options(scope.langMetaInfo, frontendFactory_old = info => new core.Frontend with foreach.Frontend {
    override val lang: LanguageMetaInfo = info
  })

  "desugaring" should "eliminate foreach loops" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(Param(Name("list"), TList(TNode("Elem")))), TUnit, Seq(Body(Seq(
        Foreach("x", Var("list"), Body(
          Assert(Eq(Var("x"), Var("x"))),
          Assert(Neq(Var("x"), Var("x")))
        )),
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(Param(Name("list"), TList(TNode("Elem")))), TUnit, Seq(Body(Seq(
        Assign(Seq("x"), PathAccess(Var("list"), ChildrenLink)),
        Assert(Eq(Var("x"), Var("x"))),
        Assert(Neq(Var("x"), Var("x"))),
      ))))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "eliminate nested foreach loops" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(Param(Name("list"), TList(TNode("Elem"))), Param(Name("list2"), TList(TNode("Elem")))), TUnit, Seq(Body(Seq(
        Foreach("x", Var("list"), Body(
          Assert(Eq(Var("x"), Var("x"))),
          Foreach("y", Var("list2"), Body(
            Assert(Eq(Var("x"), Var("y")))
          )),
          Assert(Neq(Var("x"), Var("x")))
        )),
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(Param(Name("list"), TList(TNode("Elem"))), Param(Name("list2"), TList(TNode("Elem")))), TUnit, Seq(Body(Seq(
        Assign(Seq("x"), PathAccess(Var("list"), ChildrenLink)),
        Assert(Eq(Var("x"), Var("x"))),
        Assign(Seq("y"), PathAccess(Var("list2"), ChildrenLink)),
        Assert(Eq(Var("x"), Var("y"))),
        Assert(Neq(Var("x"), Var("x"))),
      ))))
    ))

    assertDesugar(core, sugared)
  }


//  "desugaring" should "implement foreach enum semantics" in {
//    val module = Module("Test_Cast", Seq(), Seq(
//      PatternFunction(None, "integerlits", Seq(), Seq(AnnoParam(None, TNode(Exp.expTag))), Seq(Body(Seq(
//        Foreach("i", Enum(TNode(Exp.intTag)), Body(
//          Yield(Var("i"))
//        ))
//      ))))
//    ))
//
//    val input = {
//      import Exp._
//      Add(
//        Mul(
//          IntegerLit(1),
//          IntegerLit(2)
//        ),
//        Many(
//          List(
//            IntegerLit(3),
//            IntegerLit(4),
//            IntegerLit(5)
//          )
//        )
//      )
//    }
//
//    assertMatchFunModule(module, "integerlits", input, options = Options(scope.langMetaInfo, new BaseFrontend(_) with ForeachFrontend with EnumFrontend)) { matcher =>
//      assert(matcher.getAllMatches.size() == 5)
//    }
//  }

  "desugaring" should "implement foreach list semantics" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "integerlits", Seq(), TLiteral.Int, Seq(Body(Seq(
        Values("many", TNode(Exp.manyTag)),
        Foreach("i", PathAccess(Var("many"), NamedLink("exps")), Body(
          Yield(PathAccess(Cast(Var("i"), TNode(Exp.intTag)), NamedLink("value")))
        ))
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
      assert(matcher.getAllMatches.size() == 3)
      matcher.getAllMatchArrays should contain theSameElementsAs Seq(Array(3), Array(4), Array(5))
    }
  }
}
