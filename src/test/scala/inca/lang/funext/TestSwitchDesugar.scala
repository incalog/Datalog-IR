package inca.lang.funext

import inca.IncaMatchers
import inca.lang.fun.Fun._
import org.scalatest.flatspec.AnyFlatSpec

class TestSwitchDesugar extends AnyFlatSpec with IncaMatchers {

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
}
