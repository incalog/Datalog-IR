package org.inca.diff.derive

import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import org.inca.diff.RewriteChange
import org.inca.diff.macros.{diffableConstr, diffableType}

@diffableType trait Tree23
@diffableConstr case class Leaf(s: String) extends Tree23
@diffableConstr case class Node2(t1: Tree23, t2: Tree23) extends Tree23
@diffableConstr case class Node3(t1: Tree23, t2: Tree23, t3: Tree23) extends Tree23

class TestDerivedTree23 extends AnyFlatSpec with Matchers {
  implicit def leaf(s: String): Tree23 = Leaf(s)

  val n2ab: Node2 = Node2("a", "b")
  val n3abc: Node3 = Node3("a", "b", "c")

  def compareAndApply(src: Tree23, dest: Tree23): Assertion = {
    val patch = src.compareTo(dest)
    println(s"Patch:\n  $patch")
    src.applyPatch(patch) should be (Some(dest))
  }

  "diff of identical trees" should "yield empty patch" in {
    compareAndApply(
      Node2("a", "b"),
      Node2("a", "b"))
    compareAndApply(
      Node2("a", "b"),
      Node2("a", "b"))
    compareAndApply(
      n3abc,
      n3abc)
    compareAndApply(
      Node2(Node2("a", "b"), Node2("a", "b")),
      Node2(Node2("a", "b"), Node2("a", "b")))
    compareAndApply(
      Node2(n2ab, n3abc),
      Node2(n2ab, n3abc))
    compareAndApply(
      Node2(n3abc, n2ab),
      Node2(n3abc, n2ab))
    compareAndApply(
      Node2(n3abc, n3abc),
      Node2(n3abc, n3abc))
  }

  "diff of swapped trees" should "yield swap patch" in {
    compareAndApply(
      Node2("a", "b"),
      Node2("b", "a"))
    compareAndApply(
      Node2(n2ab, n3abc),
      Node2(n3abc, n2ab))
    compareAndApply(
      Node3("a", "b", "c"),
      Node3("c", "b", "a"))
    compareAndApply(
      Node3(n2ab, Node2("x1", "x2"), n3abc),
      Node3(n3abc, Node3("y1", "y2", "y3"), n2ab))
  }

  "diff of changed constructor" should "yield copy patch" in {
    compareAndApply(
      Node2("a", "b"),
      Node3("a", "foo", "b"))
    compareAndApply(
      Node2(n2ab, n3abc),
      Node3(n2ab, "foo", n3abc))
    compareAndApply(
      Node3("a", "foo", "b"),
      Node2("a", "b"))
    compareAndApply(
      Node3(n2ab, "foo", n3abc),
      Node2(n2ab, n3abc))
  }

  "diff with prefix" should "yield prefixed patch" in {
    compareAndApply(
      Node2("t", Node2("a", "b")),
      Node2("t", Node2("b", "a")))
    compareAndApply(
      Node3("t", Node2("u", "v"), Node2("w", "x")),
      Node3("t", Node2("v", "u"), Node2("w'", "x")))
  }

  it should "ensure closed changes" in {
    compareAndApply(
      Node2("a", "x"),
      Node2("a", "a"))

    compareAndApply(
      Node2("a", Node2("b", "x")),
      Node2("a", Node2("b", "b")))
  }
}