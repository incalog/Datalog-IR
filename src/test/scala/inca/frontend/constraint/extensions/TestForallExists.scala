package inca.frontend.constraint.extensions

import inca.analyzedLangs.Exp
import inca.compiler.options.ConstraintOptions
import inca.frontend.constraint.core._
import inca.frontend.constraint.extensions.forallExists.Trees._
import inca.runtime.context.QueryScope
import inca.util.Meta.Scala
import inca.util.matchers.IncaConstraintMatchers
import org.scalatest.flatspec.AnyFlatSpec

import scala.meta.XtensionQuasiquoteTerm

class TestForallExists extends AnyFlatSpec with IncaConstraintMatchers {

  implicit def name(s: String): Name = Name(s)

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))

  val dataModel = Exp.model
  val scope: QueryScope = new QueryScope(Exp.model)
  val options: ConstraintOptions = ConstraintOptions()

//  "desugaring" should "eliminate forall conds" in {
//    val sugared = Module("Test", Seq(DirectDataModel(dataModel)), Seq(), Seq(), Seq(
//      PatternFunction(None, "foo", Seq(Param("many", TNode(Exp.manyTag))), TUnit, Seq(Body(Seq(
//        Forall("x", PathAccess(Var("many"), NamedLink("exps")), Body(
//          Assert(Eq(Var("x"), Var("x")))
//        )),
//        Yield(Constant(UnitLiteral))
//      ))))
//    ))
//
//    val core = Module("Test", Seq(DirectDataModel(dataModel)), Seq(), Seq(), Seq(
//      PatternFunction(None, "forallCond", Seq(Param("many", TNode(Exp.manyTag))), TNode(Exp.expTag), Seq(Body(Seq(
//        Assign(Seq("x"),
//          PathAccess(
//            PathAccess(Var("many"), NamedLink("exps")),
//            ChildrenLink
//          )),
//        Assert(Eq(Var("x"), Var("x"))),
//        Yield(Var("x"))
//      )))),
//      PatternFunction(None, "foo", Seq(Param("many", TNode(Exp.manyTag))), TUnit, Seq(Body(Seq(
//        Assign(Seq("listSize"), PathAccess(PathAccess(Var("many"), NamedLink("exps")), SizeLink)),
//        Assign(Seq("successSize"), Count(Call("forallCond", Seq(Var("many"))))),
//        Assert(Eq(Var("listSize"), Var("successSize"))),
//        Yield(Constant(UnitLiteral))
//      ))))
//    ))
//
//    assertDesugar(core, sugared)
//  }


  "desugaring" should "implement forall list semantics" in {
    val module = Module("Test_Cast", Seq(DirectDataModel(dataModel)), Seq(), Seq(), Seq(
      PatternFunction(None, "intLists", Seq(Param("l", TList(TNode(Exp.expTag)))), TUnit, Seq(Body(Seq(
        Forall("e", Var("l"), Body(
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

    assertMatch(module, "forallCond$0", input) { matcher =>
      assert(matcher.getAllMatchArrays.size == 3)
    }

    assertMatch(module, "intLists", input) { matcher =>
      assert(matcher.getAllMatchArrays.size == 1)
    }
  }

  "desugaring" should "implement exists list semantics" in {
    val module = Module("Test_Cast", Seq(DirectDataModel(dataModel)), Seq(), Seq(), Seq(
      PatternFunction(None, "listContaining4", Seq(Param("l", TList(TNode(Exp.expTag)))), TUnit, Seq(Body(Seq(
        Exists("e", Var("l"), Body(
          Assign(Seq("i"), PathAccess(Cast(Var("e"), TNode(Exp.intTag)), NamedLink("value"))),
          Assert(Eval(Seq(EvalParam("i")), Scala(q"""i == 4""")))
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

    assertMatch(module, "existsCond$0", input) { matcher =>
      assert(matcher.getAllMatchArrays.size == 1)
    }

    assertMatch(module, "listContaining4", input) { matcher =>
      assert(matcher.getAllMatchArrays.size == 1)
    }
  }
}
