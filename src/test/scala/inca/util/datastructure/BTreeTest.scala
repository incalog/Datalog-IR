package inca.util.datastructure

import org.scalatest.funsuite.AnyFunSuite

class BTreeTest extends AnyFunSuite {

  val singleNodeSingleValueTree: BTree[Int] = {
    val tree = BTree.empty[Int](256)
    val node = BTreeNode(tree, Seq(3), Seq())
    tree.root = node
    tree
  }

  val singleNodeEntries: Seq[Int] = Seq(1, 2, 3, 6)
  val singleNodeTree: BTree[Int] = {
    val tree = BTree.empty[Int](256)
    val node = BTreeNode(tree, Seq(1, 2, 3, 6), Seq())
    tree.root = node
    tree
  }
  val twoLevelEntries: Seq[Int] = Seq(1, 2, 3, 5, 6, 9, 10)
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

  val threeLevelEntries: Seq[Int] = Seq(10, 36, 3, 6, 1, 2, 5, 7, 8, 18, 30, 12, 16, 22, 27, 33, 34,
    40, 50, 37, 38, 41, 46, 51, 56).sorted
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
    testMembership(singleNodeEntries, singleNodeTree)
  }

  test("test contains on two level tree") {
    testMembership(twoLevelEntries, twoLevelTree)
  }

  test("test contains on three level tree") {
    testMembership(threeLevelEntries, threeLevelTree)
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

  test("entries") {
    assertResult(singleNodeEntries)(singleNodeTree.entries)
    assertResult(twoLevelEntries)(twoLevelTree.entries)
    assertResult(threeLevelEntries)(threeLevelTree.entries)
  }

  test("deepCopy") {
    assertResult(singleNodeTree.entries)(singleNodeTree.deepCopy().entries)
    assertResult(twoLevelTree.entries)(twoLevelTree.deepCopy().entries)
    assertResult(threeLevelTree.entries)(threeLevelTree.deepCopy().entries)
  }

  test("lexSearch of single node tree") {
    assertResult(Seq(3))(singleNodeTree.lexSearch(2, 4))
    assertResult(Seq(2, 3))(singleNodeTree.lexSearch(1, 4))
    assertResult(Seq(1, 2, 3))(singleNodeTree.lexSearch(0, 6))
    assertResult(Seq(1, 2, 3, 6))(singleNodeTree.lexSearch(Int.MinValue, Int.MaxValue))
    assertResult(Nil)(singleNodeTree.lexSearch(-1000, 0))
    assertResult(Nil)(singleNodeTree.lexSearch(8, 1000))
  }

  test("lexSearch of two level tree") {
    assertResult(Seq(3))(twoLevelTree.lexSearch(2, 4))
    assertResult(Seq(2, 3))(twoLevelTree.lexSearch(1, 4))
    assertResult(Seq(2, 3, 5))(twoLevelTree.lexSearch(1, 6))
    assertResult(twoLevelEntries)(twoLevelTree.lexSearch(Int.MinValue, Int.MaxValue))
    assertResult(Nil)(twoLevelTree.lexSearch(-1000, 0))
    assertResult(Nil)(twoLevelTree.lexSearch(12, 1000))
  }

  test("lexSearch of three level tree") {
    assertResult(Seq(3))(threeLevelTree.lexSearch(2, 4))
    assertResult(Seq(2, 3))(threeLevelTree.lexSearch(1, 4))
    assertResult(Seq(2, 3, 5))(threeLevelTree.lexSearch(1, 6))
    assertResult(threeLevelEntries)(threeLevelTree.lexSearch(Int.MinValue, Int.MaxValue))
    assertResult(Nil)(threeLevelTree.lexSearch(-1000, 0))
    assertResult(Seq(51, 56))(threeLevelTree.lexSearch(50, 1000))
    assertResult(Nil)(threeLevelTree.lexSearch(60, 1000))
  }

  test("lexSearch of single level single value tree") {
    assertResult(Seq())(singleNodeSingleValueTree.lexSearch(3, 3))
    assertResult(Seq())(singleNodeSingleValueTree.lexSearch(1, 3))
    assertResult(Seq(3))(singleNodeSingleValueTree.lexSearch(2, 4))
    assertResult(Seq())(singleNodeSingleValueTree.lexSearch(3, 10))
  }

  test("bulkloading single node") {
    val elements = Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
    val tree = BTree.bulkLoad[Int](elements)
    assertResult(elements)(tree.entries)
  }

  test("bulkloading two levels nodes") {
    val elements = Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val tree = BTree.bulkLoad[Int](elements, 2)
    assertResult(elements)(tree.entries)
  }

  test("bulkloading three levels nodes") {
    val elements = Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
    val tree = BTree.bulkLoad[Int](elements, 2)
    assertResult(elements)(tree.entries)
  }
}
