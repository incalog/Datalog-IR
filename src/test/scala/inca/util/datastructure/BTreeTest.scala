package inca.util.datastructure

import org.scalatest.funsuite.AnyFunSuite

class BTreeTest extends AnyFunSuite {

  val singleNodeTree: BTree[Int] = {
    val tree = BTree.empty[Int](256)
    val node = BTreeNode(tree, Seq(1, 2, 3, 6), Seq())
    tree.root = node
    tree
  }
  val twoLevelTree: BTree[Int] = {
    val tree = BTree.empty[Int](3)
    val node = BTreeNode(
      tree,
      Seq(3, 6),
      Seq(
        BTreeNode(tree, Seq(1, 2), Seq()),
        BTreeNode(tree, Seq(5), Seq()),
        BTreeNode(tree, Seq(9, 10), Seq())
      )
    )
    tree.root = node
    tree
  }

  val threeLevelTree: BTree[Int] = {
    val tree = BTree.empty[Int](2)
    val node = BTreeNode(
      tree,
      Seq(10, 36),
      Seq(
        BTreeNode(
          tree,
          Seq(3, 6),
          Seq(
            BTreeNode(tree, Seq(1, 2), Seq()),
            BTreeNode(tree, Seq(5), Seq()),
            BTreeNode(tree, Seq(7, 8), Seq())
          )
        ),
        BTreeNode(
          tree,
          Seq(18, 30),
          Seq(
            BTreeNode(tree, Seq(12, 16), Seq()),
            BTreeNode(tree, Seq(22, 27), Seq()),
            BTreeNode(tree, Seq(33, 34), Seq())
          )
        ),
        BTreeNode(
          tree,
          Seq(40, 50),
          Seq(
            BTreeNode(tree, Seq(37, 38), Seq()),
            BTreeNode(tree, Seq(41, 46), Seq()),
            BTreeNode(tree, Seq(51, 56), Seq())
          )
        )
      )
    )
    tree.root = node
    tree
  }

  test("test contains on single node tree") {
    val elements = Seq(1, 2, 3, 6)
    testMembership(elements, singleNodeTree)
  }

  test("test contains on two level tree") {
    val elements = Seq(1, 2, 3, 5, 6, 9, 10)
    testMembership(elements, twoLevelTree)
  }

  test("test contains on three level tree") {
    val elements = Seq(10, 36, 3, 6, 1, 2, 5, 7, 8, 18, 30, 12, 16, 22, 27, 33, 34, 40, 50, 37, 38,
      41, 46, 51, 56)
    testMembership(elements, threeLevelTree)
  }

  private def testMembership(elements: Seq[Int], tree: BTree[Int]): Unit = {
    for (e <- elements.min to elements.max) {
      if (elements.contains(e))
        assert(tree.contains(e))
      else
        assert(!tree.contains(e))
    }
  }

  test("test size of trees") {
    assertResult(4)(singleNodeTree.size)
    assertResult(7)(twoLevelTree.size)
    assertResult(25)(threeLevelTree.size)
  }

  test("insert into empty tree") {
    val tree = BTree.empty[Int](2)
    tree.insert(4)
    assert(tree.contains(4))
    assert(!tree.contains(6))
    tree.insert(5)
    tree.insert(3)
    assert(tree.contains(5))
    assert(tree.contains(3))
    println(tree)
  }

  test("insert into full tree") {
    val tree = BTree.empty[Int](2)

    var elements: Seq[Int] = Seq()
    def insert(k: Int): Unit = {
      tree.insert(k)
      elements = elements :+ k
    }
    insert(4)
    testMembership(elements, tree)
    insert(5)
    testMembership(elements, tree)
    insert(3)
    testMembership(elements, tree)
    insert(6)
    testMembership(elements, tree)
    assertResult(Seq(4))(tree.root.keys)
    assertResult(Seq(3))(tree.root.children.head.keys)
    assertResult(Seq(5, 6))(tree.root.children(1).keys)
    insert(2)
    testMembership(elements, tree)
    insert(1)
    testMembership(elements, tree)
    assertResult(Seq(1, 2, 3))(tree.root.children.head.keys)
    insert(-1)
    testMembership(elements, tree)
  }
}
