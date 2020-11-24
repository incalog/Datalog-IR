package inca.runtime

import inca.runtime.aggregate.AugmentedAVLTree

import org.scalatest.funsuite.AnyFunSuite


class AugmentedAVLTreeTest extends AnyFunSuite {
  def newTree(): AugmentedAVLTree[Int] = new AugmentedAVLTree[Int]((x, y) =>  Math.max(x,y))
  val a = 1
  val b = 2
  val c = 3
  val d = 4
  val e = 5
  val f = 6
  val g = 7
  val h = 8
  val i = 9
  val j = 10
  val k = 11
  val l = 12

  // test cases from https://stackoverflow.com/questions/3955680/how-to-check-if-my-avl-tree-implementation-is-correct
  test("insert three elements balanced") {
    val tree = newTree()
    tree.insert(a)
    tree.insert(b)
    tree.insert(c)

    assert(tree.root.value == b)
    assert(tree.root.lhs.value == a)
    assert(tree.root.rhs.value == c)
  }

  test("insert three elements balanced 2") {
    val tree = newTree()
    tree.insert(c)
    tree.insert(b)
    tree.insert(a)

    assert(tree.root.value == b)
    assert(tree.root.lhs.value == a)
    assert(tree.root.rhs.value == c)
  }

  test("insert three elements balanced 3") {
    val tree = newTree()
    tree.insert(a)
    tree.insert(c)
    tree.insert(b)

    assert(tree.root.value == b)
    assert(tree.root.lhs.value == a)
    assert(tree.root.rhs.value == c)
  }

  test("insert three elements balanced 4") {
    val tree = newTree()
    tree.insert(c)
    tree.insert(a)
    tree.insert(b)

    assert(tree.root.value == b)
    assert(tree.root.lhs.value == a)
    assert(tree.root.rhs.value == c)
  }

  test("delete elements balanced") {
    val tree = newTree()
    tree.insert(b)
    tree.insert(c)
    tree.insert(a)
    tree.insert(d)

    tree.remove(a)

    assert(tree.root.value == c)
    assert(tree.root.lhs.value == b)
    assert(tree.root.rhs.value == d)
  }

  test("delete elements balanced 2") {
    val tree = newTree()
    tree.insert(c)
    tree.insert(b)
    tree.insert(d)
    tree.insert(a)

    tree.remove(d)

    assert(tree.root.value == b)
    assert(tree.root.lhs.value == a)
    assert(tree.root.rhs.value == c)
  }

  test("delete elements balanced 3") {
    val tree = newTree()
    tree.insert(b)
    tree.insert(d)
    tree.insert(a)
    tree.insert(c)

    tree.remove(a)

    assert(tree.root.value == c)
    assert(tree.root.lhs.value == b)
    assert(tree.root.rhs.value == d)
  }

  test("delete elements balanced 4") {
    val tree = newTree()
    tree.insert(c)
    tree.insert(a)
    tree.insert(d)
    tree.insert(b)

    tree.remove(d)

    assert(tree.root.value == b)
    assert(tree.root.lhs.value == a)
    assert(tree.root.rhs.value == c)
  }

  test("delete elements balanced 5") {
    val tree = newTree()
    tree.insert(c)
    tree.insert(b)
    tree.insert(e)
    tree.insert(d)
    tree.insert(f)
    tree.insert(a)
    tree.insert(g)

    tree.remove(a)
    tree.insert(g)

    assert(tree.root.value == e)
    assert(tree.root.lhs.value == c)
    assert(tree.root.lhs.lhs.value == b)
    assert(tree.root.lhs.rhs.value == d)
    assert(tree.root.rhs.value == f)
    assert(tree.root.rhs.rhs.value == g)
  }

  test("delete elements balanced 6") {
    val tree = newTree()
    tree.insert(e)
    tree.insert(c)
    tree.insert(f)
    tree.insert(b)
    tree.insert(d)
    tree.insert(g)
    tree.insert(a)

    tree.remove(g)
    tree.insert(a)

    assert(tree.root.value == c)
    assert(tree.root.lhs.value == b)
    assert(tree.root.lhs.lhs.value == a)
    assert(tree.root.rhs.value == e)
    assert(tree.root.rhs.lhs.value == d)
    assert(tree.root.rhs.rhs.value == f)
  }

  test("delete elements balanced 7") {
    val tree = newTree()
    tree.insert(e)
    tree.insert(c)
    tree.insert(j)
    tree.insert(a)
    tree.insert(d)
    tree.insert(h)
    tree.insert(k)
    tree.insert(g)
    tree.insert(i)
    tree.insert(l)
    tree.insert(b)
    tree.insert(f)

    tree.remove(b)
    tree.insert(f)

    assert(tree.root.value == h)
    assert(tree.root.lhs.value == e)
    assert(tree.root.lhs.lhs.value == c)
    assert(tree.root.lhs.lhs.lhs.value == a)
    assert(tree.root.lhs.lhs.rhs.value == d)
    assert(tree.root.lhs.rhs.value == g)
    assert(tree.root.lhs.rhs.lhs.value ==  f)
    assert(tree.root.rhs.value == j)
    assert(tree.root.rhs.lhs.value == i)
    assert(tree.root.rhs.rhs.value == k)
    assert(tree.root.rhs.rhs.rhs.value == l)
  }

  test("delete elements balanced 8") {
    val tree = newTree()
    tree.insert(h)
    tree.insert(c)
    tree.insert(k)
    tree.insert(b)
    tree.insert(e)
    tree.insert(i)
    tree.insert(l)
    tree.insert(a)
    tree.insert(d)
    tree.insert(f)
    tree.insert(j)
    tree.insert(g)

    tree.remove(j)
    tree.insert(g)

    assert(tree.root.value == e)
    assert(tree.root.lhs.value == c)
    assert(tree.root.lhs.lhs.value == b)
    assert(tree.root.lhs.lhs.lhs.value == a)
    assert(tree.root.lhs.rhs.value == d)
    assert(tree.root.rhs.value == h)
    assert(tree.root.rhs.lhs.value == f)
    assert(tree.root.rhs.lhs.rhs.value == g)
    assert(tree.root.rhs.rhs.value == k)
    assert(tree.root.rhs.rhs.lhs.value == i)
    assert(tree.root.rhs.rhs.rhs.value == l)
  }

  test("computed value updates correctly when inserting and removing") {
    val tree = newTree()
    tree.insert(2)
    assert(tree.root.computedValue == 2)
    tree.insert(1)
    assert(tree.root.computedValue == 2)
    tree.insert(3)
    assert(tree.root.computedValue == 3)
    tree.insert(5)
    assert(tree.root.computedValue == 5)
    tree.remove(2)
    assert(tree.root.computedValue == 5)
    tree.remove(5)
    assert(tree.root.computedValue == 3)
    tree.insert(7)
    assert(tree.root.computedValue == 7)
    tree.remove(7)
    assert(tree.root.computedValue == 3)
    tree.remove(3)
    assert(tree.root.computedValue == 1)
    tree.remove(1)
    assert(tree.root == null)
  }
}
