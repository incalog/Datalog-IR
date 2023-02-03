package inca.util.datastructure

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

class BTree[T: ClassTag](
    var root: BTreeNode[T],
    val minDegree: Int
  )(implicit val ord: Ordering[T]) {

  def size: Int =
    if (root == null) 0
    else root.size

  def contains(k: T): Boolean =
    if (root == null) false
    else root.contains(k)

  def insert(k: T): Unit =
    if (root == null) {
      root = BTreeNode(this, Seq(k), Seq())
    } else if (root.contains(k)) {
      // do nothing
    } else {
      root.insert(k)
    }

  def foreach(f: T => Unit): Unit =
    if (root == null) ()
    else root.foreach(f)

  def entries: Seq[T] =
    if (root == null) Seq()
    else root.entries

  def lexSearch(lower: T, upper: T): Seq[T] = {
    if (root == null) Seq()
    else root.lexSearch(lower, upper)
  }

  def deepCopy(): BTree[T] = {
    val treeCopy = new BTree[T](null, minDegree)
    treeCopy.root =
      if (root == null) null
      else root.deepCopy(treeCopy)
    treeCopy
  }

  override def toString: String =
    if (root == null) s"()"
    else root.toString
}
object BTree {
  // Must be set based on machines L2 Cache Size
  def GlobalMinDegree: Int = 64

  def empty[T: ClassTag](minDegree: Int = GlobalMinDegree)(implicit ord: Ordering[T]): BTree[T] =
    new BTree[T](null, minDegree)
  def apply[T: ClassTag](
      entries: Seq[T],
      minDegree: Int = GlobalMinDegree
    )(implicit ord: Ordering[T]
    ): BTree[T] = {
    bulkLoad(entries.distinct.sorted, minDegree)
  }

  // we assume sorted entries
  def bulkLoad[T: ClassTag](
      entries: Seq[T],
      minDegree: Int = GlobalMinDegree
    )(implicit ord: Ordering[T]
    ): BTree[T] = {
    val tree = BTree.empty[T](minDegree)
    if (entries.nonEmpty) {
      val root = bulkSubtree(tree, entries, minDegree)
      tree.root = root
    }
    tree
  }

  private def bulkSubtree[T: ClassTag](
      tree: BTree[T],
      entries: Seq[T],
      minDegree: Int
    )(implicit ord: Ordering[T]
    ): BTreeNode[T] = {
    val maxNumKeys = 2 * minDegree - 1

    if (entries.size <= maxNumKeys) {
      // construct leave
      BTreeNode[T](tree, entries, Seq())
    } else {
      var numKeys = maxNumKeys
      var step = (entries.size - numKeys) / (numKeys + 1)

      while (numKeys > 1 && (step < maxNumKeys / 2)) {
        numKeys -= 1
        step = (entries.size - numKeys) / (numKeys + 1)
      }

      var currentIdx = 0
      val keys = mutable.ListBuffer[T]()
      val children = mutable.ListBuffer[BTreeNode[T]]()

      for (_ <- 0 until numKeys) {
        keys += entries(currentIdx + step)
        val childrenEntries = entries.slice(currentIdx, currentIdx + step)
        children += bulkSubtree(tree, childrenEntries, minDegree)
        currentIdx += step + 1
      }
      children += bulkSubtree(tree, entries.slice(currentIdx, entries.size), minDegree)
      BTreeNode[T](tree, keys.toSeq, children.toSeq)
    }
  }

}

final class BTreeNode[T: ClassTag](
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
  def size: Int = numberOfKeys + _children.map(c => if (c != null) c.size else 0).sum

  @tailrec
  def contains(k: T): Boolean = {
    var idx = 0
    while (idx < numberOfKeys && ord.gt(k, _keys(idx)))
      idx = idx + 1
    if (idx < numberOfKeys && k == _keys(idx))
      true
    else if (isLeafNode)
      false
    else
      _children(idx).contains(k)
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

  @tailrec
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
      newNode._keys(j) = nodeToSplit._keys(j + tree.minDegree)
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

  def lexSearch(lower: T, upper: T): Seq[T] = {
    val entries = mutable.ListBuffer[T]()

    var idx = 0
    while (idx < numberOfKeys && ord.lteq(_keys(idx), lower)) {
      idx += 1
    }

    while (idx < numberOfKeys && ord.lt(_keys(idx), upper)) {
      if (!isLeafNode)
        entries ++= _children(idx).lexSearch(lower, upper)
      entries += _keys(idx)
      idx += 1
    }

    if (!isLeafNode)
      entries ++= _children(idx).lexSearch(lower, upper)

    entries.toSeq
  }

  def deepCopy(tree: BTree[T]): BTreeNode[T] = {
    new BTreeNode[T](tree, keys, children.map(_.deepCopy(tree)))
  }

  override def toString: String = {
    s"({${_keys.mkString(", ")}} -> {${_children.mkString(", ")}})"
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
