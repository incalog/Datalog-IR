package inca.runtime.aggregate

import java.util.stream

import inca.runtime.aggregate.AggregatorAssocComm.Acc
import org.eclipse.viatra.query.runtime.matchers.psystem.aggregations.IMultisetAggregationOperator

import scala.collection.mutable
import scala.jdk.CollectionConverters._



object AggregatorAssocComm {
  // TODO use a balanced tree with continuously maintained aggregate instead of MultiSet[V]
  class Acc[V]() {
    val vals: mutable.MultiSet[V] = mutable.MultiSet()
    var res: Option[V] = None
  }
}

/** An aggregator for operations that are associative and commutative */
class AggregatorAssocComm[V](val agg: Aggregation[V]) extends IMultisetAggregationOperator[V, Acc[V], V] {


  override def getShortDescription: String = agg.name
  override def getName: String = agg.name

  override def createNeutral(): Acc[V] = new Acc()
  override def isNeutral(acc: Acc[V]): Boolean = acc.vals.isEmpty

  override def update(acc: Acc[V], v: V, isInsertion: Boolean): Acc[V] = {
    if (isInsertion) {
      acc.vals += v
      acc.res = acc.res.map(agg.join(_, v))
    } else {
      acc.vals -= v
      acc.res = None
    }
    acc
  }

  override def getAggregate(acc: Acc[V]): V = acc.res match {
    case Some(value) => value
    case None =>
      val v = acc.vals.foldLeft(agg.init)(agg.join)
      acc.res = Some(v)
      v
  }

  override def aggregateStream(str: stream.Stream[V]): V =
    str.iterator().asScala.foldLeft(agg.init)(agg.join)
}

class Tree[T](op: (T, T) => T)(implicit ord: Ordering[T]) {
  var root: Node[T] = _

  def insert(value: T): Tree[T] = {
    root = insert(root, value)
    this
  }

  def insert(node: Node[T], value: T): Node[T] = {
    if (node == null) {
      Node(value, op)
    } else {
      val res = ord.compare(value, node.value)
      if (res < 0) {
        val newLhs = insert(node.lhs, value)
        node.lhs = newLhs
      } else if (res == 0) {
        node.count += 1
        // no rotations needed
        return node
      } else {
        val newRhs = insert(node.rhs, value)
        node.rhs = newRhs
      }

      // perform rotations
      node.height = Tree.computeHeight(node)
      val factor = Tree.balanceFactor(node)

      if (factor > 1) {
        // left subtree higher
        val leftFactor = Tree.balanceFactor(node.lhs)
        if (leftFactor > 0) {
          // left left case
          return rotateRight(node)
        } else {
          // left right case
          val newLhs = rotateLeft(node.lhs)
          node.lhs = newLhs
          return rotateRight(node)
        }
      } else if (factor < -1) {
        // right subtree higher
        val rightFactor = Tree.balanceFactor(node.rhs)
        if (rightFactor < 0) {
          // right right case
          return rotateLeft(node)
        } else {
          //  right left case
          val newRhs = rotateRight(node.rhs)
          node.rhs = newRhs
          return rotateLeft(node)
        }
      }
      node
    }
  }

  def remove(value: T): Tree[T] = {
    root = remove(root, value, false, false)
    this
  }

  def remove(value: T, mustBePresent: Boolean): Tree[T] = {
    root = remove(root, value, false, mustBePresent)
    this
  }

  def remove(node: Node[T], value: T, removeAll: Boolean, mustBePresent: Boolean): Node[T] = {
    var currentNode = node
    if (currentNode == null) {
      if (mustBePresent) throw new IllegalArgumentException(s"Sought node $currentNode was not present!")
      else null
    } else {
      val res = ord.compare(value, currentNode.value)
      if (res < 0) {
        val newLhs = remove(currentNode.lhs, value, removeAll, mustBePresent)
        currentNode.lhs = newLhs
      } else if (res > 0) {
        val newRhs = remove(currentNode.rhs, value, removeAll, mustBePresent)
        currentNode.rhs = newRhs
      } else {
        if (!removeAll && currentNode.count > 1) {
          currentNode.count -= 1
          return currentNode
        } else {
          if (currentNode.lhs == null || currentNode.rhs == null) {
            val child = if (currentNode.lhs == null) currentNode.rhs else currentNode.lhs
            if (child == null) {
              return null
            } else {
              val parent = currentNode.parent
              currentNode = child
              currentNode.parent = parent
            }
          } else {
            val child = leftMostLeaf(currentNode.rhs)
            currentNode.value = child.value
            currentNode.count = child.count
            val newRhs = remove(currentNode.rhs, child.value, true, mustBePresent)
            currentNode.rhs = newRhs
          }
        }
      }

      // assert node != null
      currentNode.height = Tree.computeHeight(currentNode)
      val factor = Tree.balanceFactor(currentNode)
      if (factor > 1) {
        if (Tree.balanceFactor(currentNode.lhs) > 0)
          rotateRight(currentNode)
        else {
          val newLhs = rotateLeft(currentNode.lhs)
          currentNode.lhs = newLhs
          rotateRight(currentNode)
        }
      } else if (factor < -1) {
        if (Tree.balanceFactor(currentNode.rhs) < 0) {
          rotateLeft(currentNode)
        } else {
          val newRhs = rotateRight(currentNode.rhs)
          currentNode.rhs = newRhs
          rotateLeft(currentNode)
        }
      } else currentNode
    }
  }


  def leftMostLeaf(node: Node[T]): Node[T] = {
    var current = node
    while (current.lhs != null) {
      current = current.lhs
    }
    current
  }

  def rotateLeft(node: Node[T]): Node[T] = {
    val rhs = node.rhs
    val rightLhs = rhs.lhs

    node.rhs = rightLhs
    rhs.lhs = node

    node.height = Tree.computeHeight(node)
    rhs.height = Tree.computeHeight(rhs)

    rhs
  }

  def rotateRight(node: Node[T]): Node[T] = {
    val lhs = node.lhs
    val leftRhs = lhs.rhs

    node.lhs = leftRhs
    lhs.rhs = node

    node.height = Tree.computeHeight(node)
    lhs.height = Tree.computeHeight(lhs)

    lhs
  }

  def find(value: T):  Node[T] = find(root, value)
  def find(node: Node[T], value: T):  Node[T] = {
    if (node == null) null
    else {
      val res = ord.compare(value, node.value)
      if (res < 0) find(node.lhs, value)
      else if (res == 0) node
      else find(node.rhs, value)
    }
  }
}

object Tree {

  def computeHeight[T](node: Node[T]): Int = {
    def getSetHeight(n: Node[T]): Int = if (n == null) 0 else n.height
    if (node == null) 0
    else 1 + Math.max(getSetHeight(node.lhs), getSetHeight(node.rhs))
  }

  def balanceFactor[T](node: Node[T]): Int = {
    if (node == null) {
      0
    } else {
      var res = 0
      if (node.lhs != null) res += node.lhs.height
      if (node.rhs != null) res -= node.rhs.height
      res
    }
  }
}


class Node[T](var value: T, op: (T, T) => T)(implicit ord: Ordering[T]) {

  var _parent: Node[T] = _
  private var _lhs: Node[T] = _
  private var _rhs: Node[T] = _

  def parent: Node[T] = _parent
  def lhs: Node[T] = _lhs
  def rhs: Node[T] = _rhs

  def parent_=(node: Node[T]): Unit = {
    _parent = node
  }

  def lhs_=(node: Node[T]): Unit = {
    if (_lhs ==  null ||  _lhs != node) {
      _lhs = node
      if (_lhs  != null)
        _lhs.parent = this
      lhsChanged(node)
    }
  }

  def rhs_=(node: Node[T]): Unit = {
    if (_rhs ==  null ||  _rhs != node) {
      _rhs = node
      if (_rhs  != null)
        _rhs.parent = this
      rhsChanged(node)
    }
  }

  // store the computed value based on the children
  var computedValue: T = value

  var height: Int = 1
  var count: Int = 1

  def lhsChanged(newLhs: Node[T]): Unit = recompute(newLhs, rhs)
  def rhsChanged(newRhs: Node[T]): Unit = recompute(lhs, newRhs)

  def recompute(lhs: Node[T], rhs: Node[T]): Unit = {
    val oldVal = computedValue
    var newVal = value
    if (lhs != null) {
      val left = lhs.computedValue
      newVal = op(newVal, left)
    }
    if (rhs != null) {
      val right = rhs.computedValue
      newVal = op(newVal, right)
    }
    if (oldVal != newVal) {
      computedValue = newVal
      if (parent != null)
        parent.recompute()
    }
  }

  def recompute(): Unit = recompute(lhs, rhs)

}

object Node {
  def apply[T](value: T, op: (T, T) => T)(implicit ord: Ordering[T]): Node[T] = new Node(value, op)
}