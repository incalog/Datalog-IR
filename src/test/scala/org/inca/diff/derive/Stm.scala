package org.inca.diff.derive

import org.inca.diff.macros.{cryptoHash, diffableConstr, diffableType}
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

@diffableType trait Stm
@diffableConstr case class Assign(x: String, e: Exp) extends Stm
@diffableConstr case class While(cond: Exp, body: Stm) extends Stm
@diffableConstr case class Block(content: List[Stm]) extends Stm

class TestDerivedSTM extends AnyFlatSpec with Matchers {
  def compareAndApply(src: Exp, dest: Exp): Assertion = {
    val patch = src.compareTo(dest)
    println(patch)
    src.applyPatch(patch) should be (Some(dest))
  }

  def compareAndApply(src: Stm, dest: Stm): Assertion = {
    val patch = src.compareTo(dest)
    println(patch)
    src.applyPatch(patch) should be (Some(dest))
  }

  "derived diff of Exp (contained in Stm)" should "still be correct" in {
    compareAndApply(Num(12), Num(15))
    compareAndApply(Num(12), Num(12))
    compareAndApply(Var("x"), Var("y"))
    compareAndApply(Neg(Num(12)), Bin(Neg(Num(12)), Sub, Var("x")))
    compareAndApply(Bin(Neg(Num(12)), Sub, Var("x")), Bin(Neg(Num(12)), Add, Var("x")))
    compareAndApply(Bin(Neg(Num(12)), Add, Var("x")), Bin(Neg(Num(12)), Add, Var("y")))
    compareAndApply(Bin(Neg(Num(12)), Add, Bin(Num(13), Div, Num(0))), Bin(Neg(Num(12)), Add, Bin(Num(13), Div, Num(12))))
  }

  "derived diff of Stm" should "be correct" in {
    compareAndApply(Assign("x", Num(12)), Assign("x", Num(15)))
    compareAndApply(Assign("x", Num(12)), Assign("y", Num(12)))
    compareAndApply(
      Block(List(
        Assign("x", Num(1)),
        Assign("y", Num(2)),
        While(Var("x"), Block(List(
          Assign("x", Bin(Var("x"), Add, Num(7))),
          Assign("y", Bin(Var("y"), Sub, Var("x")))
        )))
      )),
      Block(List(
        Assign("A", Num(1)),
        Assign("y", Num(2)),
        While(Var("A"), Block(List(
          Assign("A", Bin(Var("A"), Add, Num(7))),
          Assign("y", Bin(Var("y"), Sub, Var("A")))
        )))
      )))
    compareAndApply(
      Block(List(
        Assign("x", Num(1)),
        Assign("y", Num(2)),
        While(Var("x"), Block(List(
          Assign("x", Bin(Var("x"), Add, Num(7))),
          Assign("y", Bin(Var("y"), Sub, Var("x")))
        )))
      )),
      Block(List(
        Assign("y", Num(2)),
        Assign("x", Num(1)),
        While(Var("x"), Block(List(
          Assign("x", Bin(Var("x"), Add, Num(7))),
          Assign("x", Bin(Var("x"), Sub, Num(1))),
          Assign("y", Bin(Var("y"), Sub, Var("x")))
        )))
      )))
  }


}