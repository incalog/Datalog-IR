package inca.frontend.constraint.extensions

import inca.analyzedLangs.Exp
import inca.compiler.options.ConstraintOptions
import inca.frontend.constraint.core._
import inca.frontend.constraint.extensions.switch_.Trees._
import inca.runtime.context.QueryScope
import inca.util.matchers.IncaConstraintMatchers
import org.scalatest.flatspec.AnyFlatSpec

import scala.language.implicitConversions

class TestSwitch extends AnyFlatSpec with IncaConstraintMatchers {

  implicit def name(s: String): Name = Name(s)

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))
  val three = Constant(IntLiteral(3))
  val four = Constant(IntLiteral(4))

  val dataModel = Exp.model
  val scope: QueryScope = new QueryScope(Exp.model)
  val options: ConstraintOptions = ConstraintOptions()

  "desugaring" should "lift switch bodies" in {
    val sugared = Module("Test", Seq(DirectDataModel(Exp.model)), Seq(), Seq(), Seq(
      PatternFunction(None, "foo", Seq(), TUnit, Seq(Body(Seq(
        Switch(Seq(
          Body(Seq(Assert(Eq(one, two)))),
          Body(Seq(Assert(Neq(one, two)))),
          Body(Seq(Assert(Eq(three, four)))),
          Body(Seq(Assert(Neq(three, four))))
        ))
      ))))
    ))

    val core = Module("Test", Seq(DirectDataModel(Exp.model)), Seq(), Seq(), Seq(
      PatternFunction(None, "foo", Seq(), TUnit, Seq(
        Body(Seq(Assert(Eq(one, two)))),
        Body(Seq(Assert(Neq(one, two)))),
        Body(Seq(Assert(Eq(three, four)))),
        Body(Seq(Assert(Neq(three, four))))
      ))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "lift nested switch bodies" in {
    val sugared = Module("Test", Seq(DirectDataModel(Exp.model)), Seq(), Seq(), Seq(
      PatternFunction(None, "foo", Seq(), TUnit, Seq(Body(Seq(
        Switch(Seq(
          Body(Seq(Switch(Seq(
            Body(Seq(Assert(Eq(one, two)))),
            Body(Seq(Assert(Neq(one, two))))
          )))),
          Body(Seq(Switch(Seq(
            Body(Seq(Assert(Eq(three, four)))),
            Body(Seq(Assert(Neq(three, four))))
          ))))
        ))
      ))))
    ))

    val core = Module("Test", Seq(DirectDataModel(Exp.model)), Seq(), Seq(), Seq(
      PatternFunction(None, "foo", Seq(), TUnit, Seq(
        Body(Seq(Assert(Eq(one, two)))),
        Body(Seq(Assert(Neq(one, two)))),
        Body(Seq(Assert(Eq(three, four)))),
        Body(Seq(Assert(Neq(three, four))))
      ))
    ))

    assertDesugar(core, sugared)
  }

  "desugaring" should "multiply subsequent switch bodies" in {
    val sugared = Module("Test", Seq(DirectDataModel(Exp.model)), Seq(), Seq(), Seq(
      PatternFunction(None, "foo", Seq(), TUnit, Seq(Body(Seq(
        Switch(Seq(
          Body(Seq(Assert(Eq(one, two)))),
          Body(Seq(Assert(Neq(one, two))))
        )),
        Switch(Seq(
          Body(Seq(Assert(Eq(three, four)))),
          Body(Seq(Assert(Neq(three, four))))
        ))
      ))))
    ))

    val core = Module("Test", Seq(DirectDataModel(Exp.model)), Seq(), Seq(), Seq(
      PatternFunction(None, "foo", Seq(), TUnit, Seq(
        Body(Seq(Assert(Eq(one, two)), Assert(Eq(three, four)))),
        Body(Seq(Assert(Eq(one, two)), Assert(Neq(three, four)))),
        Body(Seq(Assert(Neq(one, two)), Assert(Eq(three, four)))),
        Body(Seq(Assert(Neq(one, two)), Assert(Neq(three, four))))
      ))
    ))

    assertDesugar(core, sugared)
  }


  "desugaring" should "implement switch semantics" in {
    val module = Module("Test_Cast", Seq(DirectDataModel(Exp.model)), Seq(), Seq(), Seq(
      PatternFunction(None, "integerlits", Seq(), TLiteral.Int, Seq(Body(Seq(
        Values("root", TNode(Exp.expTag)),
        Assert(Undef(PathAccess(Var("root"), ParentLink))),
        Yield(
          Call("integerlits_rec",
            Seq(Var("root"))
          )
        )
      )))),

      PatternFunction(None, "integerlits_rec", Seq(Param("e", TNode(Exp.expTag))), TLiteral.Int, Seq(Body(Seq(
        Switch(Seq(
          Body(Seq(
            Assert(InstanceOf(Var("e"), TNode(Exp.intTag))),
              Yield(PathAccess(Cast(Var("e"), TNode(Exp.intTag)), NamedLink("value")))
          )),
          Body(Seq(
            Assert(InstanceOf(Var("e"), TNode(Exp.addTag))),
            Yield(
              Call("integerlits_rec",
                Seq(PathAccess(Cast(Var("e"), TNode(Exp.addTag)), NamedLink("lhs")))
              )
            )
          )),
          Body(Seq(
            Assert(InstanceOf(Var("e"), TNode(Exp.multTag))),
            Yield(
              Call("integerlits_rec",
                Seq(PathAccess(Cast(Var("e"), TNode(Exp.multTag)), NamedLink("rhs")))
              )
            )
          ))
        ))
      ))
    ))))

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

    assertMatch(module, "integerlits_rec", input) { matcher =>
      assert(matcher.getAllMatches.size() == 7)
    }

    assertMatch(module, "integerlits", input) { matcher =>
      assert(matcher.getAllMatches.size() == 1)
      matcher.getAllMatchArrays should contain theSameElementsAs Seq(Array(2))
    }
  }
}
