package org.inca.diff.diffable.derive

import org.inca.diff.diffable.macros.{cryptoHash, diffableConstr, diffableType}
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

@diffableType trait Exp
@diffableConstr case class Num(n: Int) extends Exp
@diffableConstr case class Neg(e: Exp) extends Exp
@diffableConstr case class Bin(e1: Exp, op: BinOp, e2: Exp) extends Exp
@diffableConstr case class Var(s: String) extends Exp

@cryptoHash trait BinOp
@cryptoHash case object Add extends BinOp
@cryptoHash case object Sub extends BinOp
@cryptoHash case object Mul extends BinOp
@cryptoHash case object Div extends BinOp

class TestDerivedExp extends AnyFlatSpec with Matchers {
  def compareAndApply(src: Exp, dest: Exp): Assertion = {
    val patch = src.compareTo(dest)
    println(patch)
    src.applyPatch(patch) should be (Some(dest))
  }


  "derived diff of Exp" should "be correct" in {
    compareAndApply(Num(12), Num(15))
    compareAndApply(Num(12), Num(12))
    compareAndApply(Var("x"), Var("y"))
    compareAndApply(Neg(Num(12)), Bin(Neg(Num(12)), Sub, Var("x")))
    compareAndApply(Bin(Neg(Num(12)), Sub, Var("x")), Bin(Neg(Num(12)), Add, Var("x")))
    compareAndApply(Bin(Neg(Num(12)), Add, Var("x")), Bin(Neg(Num(12)), Add, Var("y")))
    compareAndApply(Bin(Neg(Num(12)), Add, Bin(Num(13), Div, Num(0))), Bin(Neg(Num(12)), Add, Bin(Num(13), Div, Num(12))))
  }


}