package org.inca.diff.derive

import org.inca.diff.macros.{cryptoHash, diffableConstr, diffableType}
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.implicitConversions

object Lists {
  @diffableType trait Exp
  @diffableConstr case class Num(n: Int) extends Exp
  @diffableConstr case class Many(l: List[Exp]) extends Exp
}

class TestDerivedList extends AnyFlatSpec with Matchers {
  import Lists._

  def compareAndApply(src: Exp, dest: Exp): Assertion = {
    val patch = src.compareTo(dest)
    println(s"Patch of size ${patch.size}:\n  $patch")
    src.applyPatch(patch) should be (Some(dest))
  }

  implicit def num(i: Int): Num = Num(i)
  implicit def many(l: List[Exp]): Many = Many(l)
  implicit def manyInt(l: List[Int]): Many = Many(l.map(Num(_)))


  "derived diff of Lists" should "be correct" in {
    compareAndApply(List(11, 12, 13, 14, 15), List(1, 12, 13, 14 ,15))
    compareAndApply(List(11, 12, 13, 14, 15), List(11, 12, 3, 14 ,15))
    compareAndApply(List(11, 12, 13, 14, 15), List(11, 12, 13, 14 ,5))
  }


}