//package inca.frontend.constraint.extensions
//
//import inca.IncaMatchers
//import inca.analyzedLangs.Exp
//import inca.compiler.Options
//import inca.frontend.BaseFrontend
//import inca.frontend.constraint.core._
//import inca.runtime.context.QueryScope
//import org.scalatest.flatspec.AnyFlatSpec
//
//class TestEnum extends AnyFlatSpec with IncaMatchers {
//
//  implicit def name(s: String): Name = Name(s)
//
//  val one = Constant(IntLiteral(1))
//  val two = Constant(IntLiteral(2))
//
//  val scope: QueryScope = new QueryScope(Exp.languageMetaInfo)
//  val options: Options = Options(scope.langMetaInfo, new BaseFrontend(_) with EnumFrontend)
//
//  "desugaring" should "eliminate enum" in {
//    val sugared = Module("Test", Seq(), Seq(
//      PatternFunction(None, "foo", Seq(), Seq(AnnoParam(None, TTuple(Seq(TNode(Exp.expTag), TNode(Exp.expTag))))), Seq(
//        Body(Seq(
//          Assign(Seq("x"), Enum(TNode(Exp.addTag))),
//          Assign(Seq("y"), Enum(TNode(Exp.addTag))),
//          Assert(Eq(
//            PathAccess(Var("x"), NamedLink("lhs")),
//            PathAccess(Var("y"), NamedLink("rhs")))),
//          Yield(Tuple(Seq(Var("x"), Var("y"))))
//        ))
//      ))
//    ))
//
//    val core = Module("Test", Seq(), Seq(
//      PatternFunction(None, "foo", Seq(), Seq(AnnoParam(None, TTuple(Seq(TNode(Exp.expTag), TNode(Exp.expTag))))), Seq(Body(Seq(
//        Values("enum_inca_analyzedLangs_Exp_Add", TNode(Exp.addTag)),
//        Assign(Seq("x"), Var("enum_inca_analyzedLangs_Exp_Add")),
//        Values("enum_inca_analyzedLangs_Exp_Add_0", TNode(Exp.addTag)),
//        Assign(Seq("y"), Var("enum_inca_analyzedLangs_Exp_Add_0")),
//        Assert(Eq(
//          PathAccess(Var("x"), NamedLink("lhs")),
//          PathAccess(Var("y"), NamedLink("rhs")))),
//        Yield(Tuple(Seq(Var("x"), Var("y"))))
//      ))))
//    ))
//
//    assertDesugar(core, sugared)
//  }
//
//  "desugaring" should "eliminate foreach enum" in {
//    val sugared = Module("Test", Seq(), Seq(
//      PatternFunction(None, "foo", Seq(), Seq(AnnoParam(None, TNode("Method"))), Seq(
//        Body(Seq(
//          Foreach("meth", Enum(TNode(Exp.letTag)), Body(
//            IfThenElse(
//              Eq(
//                PathAccess(Var("meth"), NamedLink("name")),
//                Constant(StringLiteral("main"))),
//              Body(Yield(Var("meth"))),
//              Seq(),
//              None),
//            FailStatement
//          ))
//        ))
//      ))
//    ))
//
//    val core = Module("Test", Seq(), Seq(
//      PatternFunction(None, "foo", Seq(), Seq(AnnoParam(None, TNode("Method"))), Seq(
//        Body(Seq(
//          Values("enum_inca_analyzedLangs_Exp_Let", TNode(Exp.letTag)),
//          Assign(Seq("meth"), Var("enum_inca_analyzedLangs_Exp_Let")),
//          Assert(Eq(
//            PathAccess(Var("meth"), NamedLink("name")),
//            Constant(StringLiteral("main")))),
//          Yield(Var("meth")),
//          FailStatement
//        )),
//        Body(Seq(
//          Values("enum_inca_analyzedLangs_Exp_Let", TNode(Exp.letTag)),
//          Assign(Seq("meth"), Var("enum_inca_analyzedLangs_Exp_Let")),
//          FailStatement
//        ))
//      ))
//    ))
//
//    assertDesugar(core, sugared, Options(scope.langMetaInfo, new BaseFrontend(_) with EnumFrontend with ForeachFrontend with IfThenElseFrontend))
//  }
//
//
//  "desugaring" should "implement enum semantics" in {
//    val module = Module("Test_Cast", Seq(), Seq(
//      PatternFunction(None, "integerlits", Seq(), Seq(AnnoParam(None, TNode(Exp.expTag))), Seq(Body(Seq(
//        Assign(Seq("i"), Enum(TNode(Exp.intTag))),
//        Yield(Var("i"))
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
//    assertMatchFunModule(module, "integerlits", input) { matcher =>
//      assert(matcher.getAllMatches.size() == 5)
//    }
//  }
//}
