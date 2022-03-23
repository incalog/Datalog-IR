package inca.util.datastructure

import scala.collection.mutable
import scala.reflect.ClassTag

class BTree[T: ClassTag](
    var root: BTreeNode[T],
    val minDegree: Int
  )(implicit val ord: Ordering[T]) {

  def size: Int = root.size

  def contains(k: T): Boolean =
    if (root == null) false
    else root.contains(k)

  def insert(k: T): Unit =
    if (root == null) {
      root = BTreeNode(this, Seq(k), Seq())
    } else
      root.insert(k)

  def foreach(f: T => Unit): Unit = root.foreach(f)
  def entries: Seq[T] = root.entries

  def deepCopy(): BTree[T] = {
    val treeCopy = new BTree[T](null, minDegree)
    treeCopy.root = root.deepCopy(treeCopy)
    treeCopy
  }

  override def toString: String =
    if (root == null) s"()"
    else root.toString
}
object BTree {
  def empty[T: ClassTag](minDegree: Int = 256)(implicit ord: Ordering[T]): BTree[T] =
    new BTree[T](null, minDegree)
}

class BTreeNode[T: ClassTag](
    var tree: BTree[T],
    val initKeys: Seq[T],
    val initChildren: Seq[BTreeNode[T]]
  )(implicit val ord: Ordering[T]) {

  var _keys: mutable.ArraySeq[T] =
    mutable.ArraySeq.make(Array.ofDim(2 * tree.minDegree - 1))

  var _children: mutable.ArraySeq[BTreeNode[T]] =
    mutable.ArraySeq.make(Array.ofDim(2 * tree.minDegree))

  initKeys.zipWithIndex.foreach { case (k, i) =>
    _keys(i) = k
  }
  initChildren.zipWithIndex.foreach { case (c, i) =>
    _children(i) = c
  }

  var _numberOfKeys: Int = initKeys.size
  var _isLeafNode: Boolean = initChildren.isEmpty

  private def incKeyCount(): Unit = {
    _numberOfKeys += 1
  }

  def keys: Seq[T] = _keys.take(numberOfKeys).toSeq
  def children: Seq[BTreeNode[T]] =
    if (isLeafNode) Seq()
    else _children.take(numberOfKeys + 1).toSeq

  def isLeafNode: Boolean = _isLeafNode
  def numberOfKeys: Int = _numberOfKeys
  def size: Int = numberOfKeys + children.map(c => if (c != null) c.size else 0).sum

  def contains(k: T): Boolean = {
    var idx = 0
    while (idx < numberOfKeys && ord.gt(k, keys(idx)))
      idx = idx + 1
    if (idx < numberOfKeys && k == keys(idx))
      true
    else if (isLeafNode)
      false
    else
      children(idx).contains(k)
  }

  def insert(k: T): Unit = {
    if (tree.root.numberOfKeys == 2 * tree.minDegree - 1) {
      val oldRoot = tree.root
      val newRoot =
        new BTreeNode(tree, Seq(), Seq(oldRoot))
      tree.root = newRoot
      newRoot._isLeafNode = false
      newRoot._numberOfKeys = 0
      newRoot.split(0)
      newRoot.insertNotFull(k)
    } else {
      tree.root.insertNotFull(k)
    }
  }

  def insertNotFull(k: T): Unit = {
    var idx = numberOfKeys - 1
    if (isLeafNode) {
      while (idx >= 0 && ord.lt(k, _keys(idx))) {
        _keys(idx + 1) = _keys(idx)
        idx -= 1
      }
      _keys(idx + 1) = k
      incKeyCount()
    } else {
      while (idx >= 0 && ord.lt(k, _keys(idx))) {
        idx -= 1
      }
      idx += 1
      val child = _children(idx)
      if (child.numberOfKeys == 2 * tree.minDegree - 1) {
        split(idx)
        if (ord.gt(k, _keys(idx)))
          idx += 1
      }
      // could now be different node than stored in child
      _children(idx).insertNotFull(k)
    }
  }

  def split(idx: Int): Unit = {
    // we split the child node positioned at index idx
    val nodeToSplit = _children(idx)

    val newNode = new BTreeNode[T](tree, Seq(), Seq())
    newNode._isLeafNode = nodeToSplit.isLeafNode
    newNode._numberOfKeys = tree.minDegree - 1
    for (j <- 0 until tree.minDegree - 1) {
      newNode._keys(j) = nodeToSplit.keys(j + tree.minDegree)
    }
    if (!nodeToSplit.isLeafNode) {
      for (j <- 0 until tree.minDegree) {
        newNode._children(j) = nodeToSplit._children(j + tree.minDegree)
      }
    }
    nodeToSplit._numberOfKeys = tree.minDegree - 1
    for (j <- (idx + 1 to numberOfKeys).reverse) {
      _children(j + 1) = _children(j)
    }
    _children(idx + 1) = newNode
    for (j <- (idx until numberOfKeys).reverse) {
      _keys(j + 1) = _keys(j)
    }
    _keys(idx) = nodeToSplit._keys(tree.minDegree - 1)
    incKeyCount()
  }

  def entries: Seq[T] = {
    val res = mutable.ListBuffer[T]()
    foreach { k =>
      res.append(k)
    }
    res.toSeq
  }

  def foreach(f: T => Unit): Unit = {
    for (i <- 0 until numberOfKeys) {
      if (!isLeafNode) {
        _children(i).foreach(f)
      }
      f(_keys(i))
    }
    if (!isLeafNode) {
      _children(numberOfKeys).foreach(f)
    }
  }

  def deepCopy(tree: BTree[T]): BTreeNode[T] = {
    val childrenCopies = children.map { child =>
      child.deepCopy(tree)
    }.toSeq
    new BTreeNode[T](tree, keys, childrenCopies)
  }

  override def toString: String = {
    s"({${keys.mkString(", ")}} -> {${children.mkString(", ")}})"
  }
}

object BTreeNode {
  def apply[T: ClassTag](
      tree: BTree[T],
      keys: Seq[T],
      children: Seq[BTreeNode[T]]
    )(implicit ord: Ordering[T]
    ): BTreeNode[T] = {
    new BTreeNode[T](tree, keys, children)
  }
}
