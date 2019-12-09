package org.inca.diff.javareflect

import java.nio.charset.StandardCharsets

import org.inca.diff.javareflect.GenericReflectionDiff._
import org.inca.diff.WithCachedCryptoHash
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import GenericReflectionCryptoHashOracle.digest

class TestGenericReflectionTree23DiffIndexSubtreesOracle extends TestGenericReflectionTree23Diff(GenericReflectionIndexSubtreesOracle)

class TestGenericReflectionTree23DiffCryptoHashOracle extends TestGenericReflectionTree23Diff(GenericReflectionCryptoHashOracle)

class TestGenericReflectionTree23Diff(mkOracle: MkGenericReflectionOracle) extends AnyFlatSpec with Matchers {

  trait Tree extends StructuralDiff
  case class Leaf(s: String) extends Tree
  case class Node2(t1: Tree, t2: Tree) extends Tree
  case class Node3(t1: Tree, t2: Tree, t3: Tree) extends Tree

  val cLeaf: Class[Leaf] = classOf[Leaf]
  val cNode2: Class[Node2] = classOf[Node2]
  val cNode3: Class[Node3] = classOf[Node3]

  implicit def leaf(s: String): Tree = Leaf(s)

  val n2ab: Node2 = Node2("a", "b")
  val n3abc: Node3 = Node3("a", "b", "c")

  implicit val implicit_mkOracle: MkGenericReflectionOracle = mkOracle
  
  "diff of identical trees" should "yield empty patch" in {
    val emptyPatch: PartialFunction[Any,_] = {
      case Hole(Change(Hole(i1), Hole(i2))) if i1==i2 =>
    }
    
    diffTree(Node2("a", "b"), Node2("a", "b")) should matchPattern (emptyPatch)
    diffTree(n3abc, n3abc) should matchPattern (emptyPatch)
    diffTree(Node2(n2ab, n2ab), Node2(n2ab, n2ab)) should matchPattern (emptyPatch)
    diffTree(Node2(n2ab, n3abc), Node2(n2ab, n3abc)) should matchPattern (emptyPatch)
    diffTree(Node2(n3abc, n2ab), Node2(n3abc, n2ab)) should matchPattern (emptyPatch)
    diffTree(Node2(n3abc, n3abc), Node2(n3abc, n3abc)) should matchPattern (emptyPatch)
  }

  "diff of swapped trees" should "yield swap patch" in {
    val swapPatch: PartialFunction[Any,_] = {
      case Hole(Change(NodeC(`cNode2`, Seq(Hole(i1), Hole(j1))), NodeC(`cNode2`, Seq(Hole(j2), Hole(i2))))) if i1==i2 && j1==j2 =>
      case Hole(Change(NodeC(`cNode3`, Seq(Hole(i1), _, Hole(j1))), NodeC(`cNode3`, Seq(Hole(j2), _, Hole(i2))))) if i1==i2 && j1==j2 =>
    }

    diffTree(Node2("a", "b"), Node2("b", "a")) should matchPattern (swapPatch)
    diffTree(Node2(n2ab, n3abc), Node2(n3abc, n2ab)) should matchPattern (swapPatch)
    diffTree(Node3("a", "b", "c"), Node3("c", "b", "a")) should matchPattern (swapPatch)
    diffTree(Node3(n2ab, Node2("x1", "x2"), n3abc), Node3(n3abc, Node3("y1", "y2", "y3"), n2ab)) should matchPattern (swapPatch)
  }

  "diff of changed constructor" should "yield copy patch" in {
    val copyPatch: PartialFunction[Any,_] = {
      case Hole(Change(NodeC(`cNode2`, Seq(Hole(i1), Hole(j1))), NodeC(`cNode3`, Seq(Hole(i2), _, Hole(j2))))) if i1==i2 && j1==j2 =>
      case Hole(Change(NodeC(`cNode3`, Seq(Hole(i2), _, Hole(j2))), NodeC(`cNode2`, Seq(Hole(i1), Hole(j1))))) if i1==i2 && j1==j2 =>
    }

    diffTree(Node2("a", "b"), Node3("a", "foo", "b")) should matchPattern (copyPatch)
    diffTree(Node2(n2ab, n3abc), Node3(n2ab, "foo", n3abc)) should matchPattern (copyPatch)
    diffTree(Node3("a", "foo", "b"), Node2("a", "b")) should matchPattern (copyPatch)
    diffTree(Node3(n2ab, "foo", n3abc), Node2(n2ab, n3abc)) should matchPattern (copyPatch)
  }

  "diff with prefix" should "yield prefixed patch" in {
    diffTree(Node2("t", Node2("a", "b")), Node2("t", Node2("b", "a"))) should matchPattern {
      case NodeC(`cNode2`, Seq(
        Hole(Change(Hole(k1), Hole(k2))),
        Hole(Change(NodeC(`cNode2`, Seq(Hole(i1), Hole(j1))), NodeC(`cNode2`, Seq(Hole(j2), Hole(i2)))))
      )) if i1==i2 && j1==j2 && k1==k2 =>
    }

    diffTree(Node3("t", Node2("u", "v"), Node2("w", "x")), Node3("t", Node2("v", "u"), Node2("w'", "x"))) should matchPattern {
      case NodeC(`cNode3`, Seq(
        Hole(Change(Hole(k1), Hole(k2))),
        Hole(Change(NodeC(`cNode2`, Seq(Hole(i1), Hole(j1))), NodeC(`cNode2`, Seq(Hole(j2), Hole(i2))))),
        NodeC(`cNode2`, Seq(
          NodeC(`cLeaf`, Seq(Hole(Change(ValC("w"), ValC("w'"))))),
          Hole(Change(Hole(l1), Hole(l2))),
        ))
      )) if i1==i2 && j1==j2 && k1==k2 && l1==l2 =>
    }
  }

  it should "ensure closed changes" in {
    diffTree(Node2("a", "x"), Node2("a", "a")) should matchPattern {
      case Hole(Change(NodeC(`cNode2`, Seq(Hole(i1), _)), NodeC(`cNode2`, Seq(Hole(i2), Hole(i3))))) if i1==i2 && i2==i3 =>
    }

    diffTree(Node2("a", Node2("b", "x")), Node2("a", Node2("b", "b"))) should matchPattern {
      case NodeC(`cNode2`, Seq(
        Hole(Change(Hole(k1), Hole(k2))),
        Hole(Change(NodeC(`cNode2`, Seq(Hole(i1), _)), NodeC(`cNode2`, Seq(Hole(i2), Hole(i3)))))
      )) if k1==k2 && i1==i2 && i2==i3 =>
    }
  }
}
