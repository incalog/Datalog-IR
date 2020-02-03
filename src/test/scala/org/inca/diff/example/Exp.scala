package org.inca.diff.example

import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestExampleExp extends AnyFlatSpec with Matchers {
  def compareAndApply(src: Exp, dest: Exp): Assertion = {
    println("Comparing:")
    println(s"  $src")
    println(s"  $dest")

    val patch = src.compareTo(dest)
    println(s"Patch:\n  $patch")

    val changeset = src.changeset(dest)
    println("Changeset:")
    changeset.foreach(c => println("  " + c))
    println()

    src.applyPatch(patch) should be (Some(dest))
  }


  "derived diff of Exp" should "be correct" in {
    compareAndApply(Num(12), Num(15))
    compareAndApply(Num(12), Num(12))
    compareAndApply(Add(Num(12), Num(13)), Mul(Num(13), Num(14)))
    compareAndApply(Add(Num(12), Num(13)), Add(Num(13), Num(12)))
    compareAndApply(Add(Num(12), Num(13)), Add(Num(12), Num(99)))
    compareAndApply(Add(Num(12), Num(13)), Mul(Num(12), Num(13)))
  }


}