package org.inca.diff.example

import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import SimpleExp._

class TestExampleSimpleExp extends AnyFlatSpec with Matchers {
  def compareAndApply(src: Exp, dest: Exp) = {
    println("Comparing:")
    println(s"  $src")
    println(s"  $dest")

    val changeset = diff(src, dest)
    println("Changeset:")
    changeset.foreach(c => println("  " + c))
    println()
  }


  "derived diff of Exp" should "be correct" in {
    compareAndApply(Num(12), Num(15))
    compareAndApply(Num(12), Num(12))
    compareAndApply(Add(Num(12), Num(13)), Mul(Num(13), Num(14)))
    compareAndApply(Add(Num(12), Num(13)), Add(Num(13), Num(12)))
    compareAndApply(Add(Num(12), Num(13)), Add(Num(12), Num(99)))
    compareAndApply(Add(Num(12), Num(13)), Mul(Num(12), Num(13)))

    compareAndApply(Add(Num(12), Num(12)), Add(Num(12), Num(13)))
    compareAndApply(Add(Num(12), Num(13)), Add(Num(12), Num(12)))
    compareAndApply(Add(Num(12), Num(12)), Mul(Num(12), Num(13)))
    compareAndApply(Add(Num(12), Num(13)), Mul(Num(12), Num(12)))
  }


}