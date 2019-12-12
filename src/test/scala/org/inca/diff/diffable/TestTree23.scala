package org.inca.diff.diffable

import org.inca.diff.diffable.example.{Leaf, Node2, Node3, Tree23, Tree23Hole}
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers



class TestTree23 extends AnyFlatSpec with Matchers {
  implicit def leaf(s: String): Tree23 = Leaf(s)

  val n2ab: Node2 = Node2("a", "b")
  val n3abc: Node3 = Node3("a", "b", "c")

  implicit val implicit_mkOracle = DiffableCryptoHashOracle

  def compareAndApply(src: Tree23, dest: Tree23): Assertion = {
    val patch = src.compareTo(dest)
    src.applyPatch(patch).get should be (dest)
  }

  "diff of identical trees" should "yield empty patch" in {
    val emptyPatch: PartialFunction[Any,_] = {
      case Tree23Hole(Change(Tree23Hole(i1), Tree23Hole(i2))) if i1==i2 =>
    }

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
    val swapPatch: PartialFunction[Any,_] = {
      case Tree23Hole(Change(Node2(Tree23Hole(i1), Tree23Hole(j1)), Node2(Tree23Hole(j2), Tree23Hole(i2)))) if i1==i2 && j1==j2 =>
      case Tree23Hole(Change(Node3(Tree23Hole(i1), _, Tree23Hole(j1)), Node3(Tree23Hole(j2), _, Tree23Hole(i2)))) if i1==i2 && j1==j2 =>
    }

    Node2("a", "b").compareTo(Node2("b", "a")) should matchPattern (swapPatch)
    Node2(n2ab, n3abc).compareTo(Node2(n3abc, n2ab)) should matchPattern (swapPatch)
    Node3("a", "b", "c").compareTo(Node3("c", "b", "a")) should matchPattern (swapPatch)
    Node3(n2ab, Node2("x1", "x2"), n3abc).compareTo(Node3(n3abc, Node3("y1", "y2", "y3"), n2ab)) should matchPattern (swapPatch)

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
    val copyPatch: PartialFunction[Any,_] = {
      case Tree23Hole(Change(Node2(Tree23Hole(i1), Tree23Hole(j1)), Node3(Tree23Hole(i2), _, Tree23Hole(j2)))) if i1==i2 && j1==j2 =>
      case Tree23Hole(Change(Node3(Tree23Hole(i2), _, Tree23Hole(j2)), Node2(Tree23Hole(i1), Tree23Hole(j1)))) if i1==i2 && j1==j2 =>
    }

    Node2("a", "b").compareTo(Node3("a", "foo", "b")) should matchPattern (copyPatch)
    Node2(n2ab, n3abc).compareTo(Node3(n2ab, "foo", n3abc)) should matchPattern (copyPatch)
    Node3("a", "foo", "b").compareTo(Node2("a", "b")) should matchPattern (copyPatch)
    Node3(n2ab, "foo", n3abc).compareTo(Node2(n2ab, n3abc)) should matchPattern (copyPatch)

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
    Node2("t", Node2("a", "b")).compareTo(Node2("t", Node2("b", "a"))) should matchPattern {
      case Node2(
        Tree23Hole(Change(Tree23Hole(k1), Tree23Hole(k2))),
        Tree23Hole(Change(Node2(Tree23Hole(i1), Tree23Hole(j1)), Node2(Tree23Hole(j2), Tree23Hole(i2))))
      ) if i1==i2 && j1==j2 && k1==k2 =>
    }
    compareAndApply(
      Node2("t", Node2("a", "b")),
      Node2("t", Node2("b", "a")))

    Node3("t", Node2("u", "v"), Node2("w", "x")).compareTo(Node3("t", Node2("v", "u"), Node2("w'", "x"))) should matchPattern {
      case Node3(
        Tree23Hole(Change(Tree23Hole(k1), Tree23Hole(k2))),
        Tree23Hole(Change(Node2(Tree23Hole(i1), Tree23Hole(j1)), Node2(Tree23Hole(j2), Tree23Hole(i2)))),
        Node2(
          Tree23Hole(Change(Leaf("w"), Leaf("w'"))),
          Tree23Hole(Change(Tree23Hole(l1), Tree23Hole(l2))),
        )
      ) if i1==i2 && j1==j2 && k1==k2 && l1==l2 =>
    }
    compareAndApply(
      Node3("t", Node2("u", "v"), Node2("w", "x")),
      Node3("t", Node2("v", "u"), Node2("w'", "x")))
  }

  it should "ensure closed changes" in {
    Node2("a", "x").compareTo(Node2("a", "a")) should matchPattern {
      case Tree23Hole(Change(Node2(Tree23Hole(i1), _), Node2(Tree23Hole(i2), Tree23Hole(i3)))) if i1==i2 && i2==i3 =>
    }
    compareAndApply(
      Node2("a", "x"),
      Node2("a", "a"))

    Node2("a", Node2("b", "x")).compareTo(Node2("a", Node2("b", "b"))) should matchPattern {
      case Node2(
        Tree23Hole(Change(Tree23Hole(k1), Tree23Hole(k2))),
        Tree23Hole(Change(Node2(Tree23Hole(i1), _), Node2(Tree23Hole(i2), Tree23Hole(i3))))
      ) if k1==k2 && i1==i2 && i2==i3 =>
    }
    compareAndApply(
      Node2("a", Node2("b", "x")),
      Node2("a", Node2("b", "b")))
  }
}
