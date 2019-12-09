package org.inca.diff

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import Tree23._


class TestTree23IndexSubtreesOracle extends TestTree23(IndexSubtreesOracle23)
class TestTree23CryptoHashOracle extends TestTree23(CryptoHashOracle23)

class TestTree23(mkOracle: MkOracle23) extends AnyFlatSpec with Matchers {
  
  implicit def leaf(s: String): Tree23 = Leaf(s)

  val n2ab = Node2("a", "b")
  val n3abc = Node3("a", "b", "c")

  implicit val implicit_mkOracle = mkOracle
  
  "diff of identical trees" should "yield empty patch" in {
    val emptyPatch: PartialFunction[Any,_] = {
      case Hole(Change23(Hole(i1), Hole(i2))) if i1==i2 =>
    }
    
    diffTree23(Node2("a", "b"), Node2("a", "b")) should matchPattern (emptyPatch)
    diffTree23(n3abc, n3abc) should matchPattern (emptyPatch)
    diffTree23(Node2(n2ab, n2ab), Node2(n2ab, n2ab)) should matchPattern (emptyPatch)
    diffTree23(Node2(n2ab, n3abc), Node2(n2ab, n3abc)) should matchPattern (emptyPatch)
    diffTree23(Node2(n3abc, n2ab), Node2(n3abc, n2ab)) should matchPattern (emptyPatch)
    diffTree23(Node2(n3abc, n3abc), Node2(n3abc, n3abc)) should matchPattern (emptyPatch)
  }

  "diff of swapped trees" should "yield swap patch" in {
    val swapPatch: PartialFunction[Any,_] = {
      case Hole(Change23(Node2C(Hole(i1), Hole(j1)), Node2C(Hole(j2), Hole(i2)))) if i1==i2 && j1==j2 =>
      case Hole(Change23(Node3C(Hole(i1), _, Hole(j1)), Node3C(Hole(j2), _, Hole(i2)))) if i1==i2 && j1==j2 =>
    }

    diffTree23(Node2("a", "b"), Node2("b", "a")) should matchPattern (swapPatch)
    diffTree23(Node2(n2ab, n3abc), Node2(n3abc, n2ab)) should matchPattern (swapPatch)
    diffTree23(Node3("a", "b", "c"), Node3("c", "b", "a")) should matchPattern (swapPatch)
    diffTree23(Node3(n2ab, Node2("x1", "x2"), n3abc), Node3(n3abc, Node3("y1", "y2", "y3"), n2ab)) should matchPattern (swapPatch)
  }

  "diff of changed constructor" should "yield copy patch" in {
    val copyPatch: PartialFunction[Any,_] = {
      case Hole(Change23(Node2C(Hole(i1), Hole(j1)), Node3C(Hole(i2), _, Hole(j2)))) if i1==i2 && j1==j2 =>
      case Hole(Change23(Node3C(Hole(i2), _, Hole(j2)), Node2C(Hole(i1), Hole(j1)))) if i1==i2 && j1==j2 =>
    }

    diffTree23(Node2("a", "b"), Node3("a", "foo", "b")) should matchPattern (copyPatch)
    diffTree23(Node2(n2ab, n3abc), Node3(n2ab, "foo", n3abc)) should matchPattern (copyPatch)
    diffTree23(Node3("a", "foo", "b"), Node2("a", "b")) should matchPattern (copyPatch)
    diffTree23(Node3(n2ab, "foo", n3abc), Node2(n2ab, n3abc)) should matchPattern (copyPatch)
  }

  "diff with prefix" should "yield prefixed patch" in {
    diffTree23(Node2("t", Node2("a", "b")), Node2("t", Node2("b", "a"))) should matchPattern {
      case Node2C(
        Hole(Change23(Hole(k1), Hole(k2))),
        Hole(Change23(Node2C(Hole(i1), Hole(j1)), Node2C(Hole(j2), Hole(i2))))
      ) if i1==i2 && j1==j2 && k1==k2 =>
    }

    diffTree23(Node3("t", Node2("u", "v"), Node2("w", "x")), Node3("t", Node2("v", "u"), Node2("w'", "x"))) should matchPattern {
      case Node3C(
        Hole(Change23(Hole(k1), Hole(k2))),
        Hole(Change23(Node2C(Hole(i1), Hole(j1)), Node2C(Hole(j2), Hole(i2)))),
        Node2C(
          Hole(Change23(LeafC("w"), LeafC("w'"))),
          Hole(Change23(Hole(l1), Hole(l2))),
        )
      ) if i1==i2 && j1==j2 && k1==k2 && l1==l2 =>
    }
  }

  it should "ensure closed changes" in {
    diffTree23(Node2("a", "x"), Node2("a", "a")) should matchPattern {
      case Hole(Change23(Node2C(Hole(i1), _), Node2C(Hole(i2), Hole(i3)))) if i1==i2 && i2==i3 =>
    }

    diffTree23(Node2("a", Node2("b", "x")), Node2("a", Node2("b", "b"))) should matchPattern {
      case Node2C(
        Hole(Change23(Hole(k1), Hole(k2))),
        Hole(Change23(Node2C(Hole(i1), _), Node2C(Hole(i2), Hole(i3))))
      ) if k1==k2 && i1==i2 && i2==i3 =>
    }
  }
}
