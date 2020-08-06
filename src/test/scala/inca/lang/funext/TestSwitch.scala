package inca.lang.funext

import inca.IncaMatchers
import inca.analyzedLangs.Exp
import inca.lang.fun.Fun._
import inca.runtime.context.QueryScope
import org.scalatest.flatspec.AnyFlatSpec

class TestSwitch extends AnyFlatSpec with IncaMatchers {

  val one = Constant(IntLiteral(1))
  val two = Constant(IntLiteral(2))
  val three = Constant(IntLiteral(3))
  val four = Constant(IntLiteral(4))

  "desugaring" should "lift switch bodies" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
        Switch(Seq(
          Body(Seq(Assert(Eq(one, two)))),
          Body(Seq(Assert(Neq(one, two)))),
          Body(Seq(Assert(Eq(three, four)))),
          Body(Seq(Assert(Neq(three, four))))
        ))
      ))))
    ))

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(
        Body(Seq(Assert(Eq(one, two)))),
        Body(Seq(Assert(Neq(one, two)))),
        Body(Seq(Assert(Eq(three, four)))),
        Body(Seq(Assert(Neq(three, four))))
      ))
    ))

    assertDesugar(core, sugared, Switch)
  }

  "desugaring" should "lift nested switch bodies" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
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

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(
        Body(Seq(Assert(Eq(one, two)))),
        Body(Seq(Assert(Neq(one, two)))),
        Body(Seq(Assert(Eq(three, four)))),
        Body(Seq(Assert(Neq(three, four))))
      ))
    ))

    assertDesugar(core, sugared, Switch)
  }

  "desugaring" should "multiply subsequent switch bodies" in {
    val sugared = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(Body(Seq(
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

    val core = Module("Test", Seq(), Seq(
      PatternFunction(None, "foo", Seq(), Seq(), Seq(
        Body(Seq(Assert(Eq(one, two)), Assert(Eq(three, four)))),
        Body(Seq(Assert(Eq(one, two)), Assert(Neq(three, four)))),
        Body(Seq(Assert(Neq(one, two)), Assert(Eq(three, four)))),
        Body(Seq(Assert(Neq(one, two)), Assert(Neq(three, four))))
      ))
    ))

    assertDesugar(core, sugared, Switch)
  }


  val scope = new QueryScope(Exp.languageMetaInfo)

  "desugaring" should "implement switch semantics" in {
    val module = Module("Test_Cast", Seq(), Seq(
      PatternFunction(None, "integerlits", Seq(), Seq(AnnoParam(None, TInt)), Seq(Body(Seq(
        Values("root", TNode(Exp.expTag)),
        Assert(Undef(PathAccess(Var("root").typed(TNode(Exp.expTag)), ParentLink).typed(TAnyLinked))),
        Yield(
          Call("integerlits_rec",
            Seq(Var("root")),
            transitive = false, count = false
          )
        )
      )))),

      PatternFunction(None, "integerlits_rec", Seq(Param("e", Some(TNode(Exp.expTag)))), Seq(AnnoParam(None, TInt)), Seq(Body(Seq(
        Switch(Seq(
          Body(Seq(
            Assert(InstanceOf(Var("e"), TNode(Exp.intTag))),
              Yield(PathAccess(Var("e"), NamedLink(TNode(Exp.intTag), "value")).typed(TInt))
          )),
          Body(Seq(
            Assert(InstanceOf(Var("e"), TNode(Exp.addTag))),
            Yield(
              Call("integerlits_rec",
                Seq(PathAccess(Var("e"), NamedLink(TNode(Exp.addTag), "lhs")).typed(TNode(Exp.expTag))),
                transitive = false, count = false
              )
            )
          )),
          Body(Seq(
            Assert(InstanceOf(Var("e"), TNode(Exp.multTag))),
            Yield(
              Call("integerlits_rec",
                Seq(PathAccess(Var("e"), NamedLink(TNode(Exp.multTag), "rhs")).typed(TNode(Exp.expTag))),
                transitive = false, count = false
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

    assertMatch(module, "generated_helper_undefpath_ParentLink", input, scope, IfThenElse) { matcher =>
      assert(matcher.getAllMatches.size() == 7)
    }

    assertMatch(module, "integerlits_rec", input, scope, IfThenElse) { matcher =>
      assert(matcher.getAllMatches.size() == 7)
    }

    assertMatch(module, "integerlits", input, scope, IfThenElse) { matcher =>
      assert(matcher.getAllMatches.size() == 1)
      matcher.getAllMatchArrays should contain theSameElementsAs Seq(Array(2))
    }
  }
}
