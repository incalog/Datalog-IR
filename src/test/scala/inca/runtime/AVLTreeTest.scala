package inca.runtime

import inca.runtime.aggregate.{Node, Tree}

import org.scalatest.funsuite.AnyFunSuite


class AVLTreeTest extends AnyFunSuite {
  test("computed value updates correctly when inserting and removing") {

    val tree = new Tree[Int]((x, y) =>  Math.max(x,y))
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
  }
}
