package inca.runtime.aggregate

class AugmentedAVLTree[T](op: (T, T) => T)(implicit ord: Ordering[T]) {
  var root: AugmentedAVLNode[T] = _

  def insert(value: T): AugmentedAVLTree[T] = {
    root = insert(root, value)
    this
  }

  def insert(node: AugmentedAVLNode[T], value: T): AugmentedAVLNode[T] = {
    if (node == null) {
      AugmentedAVLNode(value, op)
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
      node.height = AugmentedAVLTree.computeHeight(node)
      val factor = AugmentedAVLTree.balanceFactor(node)

      if (factor > 1) {
        // left subtree higher
        val leftFactor = AugmentedAVLTree.balanceFactor(node.lhs)
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
        val rightFactor = AugmentedAVLTree.balanceFactor(node.rhs)
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

  def remove(value: T): AugmentedAVLTree[T] = {
    root = remove(root, value, false, false)
    this
  }

  def remove(value: T, mustBePresent: Boolean): AugmentedAVLTree[T] = {
    root = remove(root, value, false, mustBePresent)
    this
  }

  def remove(node: AugmentedAVLNode[T], value: T, removeAll: Boolean, mustBePresent: Boolean): AugmentedAVLNode[T] = {
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
      currentNode.height = AugmentedAVLTree.computeHeight(currentNode)
      val factor = AugmentedAVLTree.balanceFactor(currentNode)
      if (factor > 1) {
        if (AugmentedAVLTree.balanceFactor(currentNode.lhs) > 0)
          rotateRight(currentNode)
        else {
          val newLhs = rotateLeft(currentNode.lhs)
          currentNode.lhs = newLhs
          rotateRight(currentNode)
        }
      } else if (factor < -1) {
        if (AugmentedAVLTree.balanceFactor(currentNode.rhs) < 0) {
          rotateLeft(currentNode)
        } else {
          val newRhs = rotateRight(currentNode.rhs)
          currentNode.rhs = newRhs
          rotateLeft(currentNode)
        }
      } else currentNode
    }
  }


  def leftMostLeaf(node: AugmentedAVLNode[T]): AugmentedAVLNode[T] = {
    var current = node
    while (current.lhs != null) {
      current = current.lhs
    }
    current
  }

  def rotateLeft(node: AugmentedAVLNode[T]): AugmentedAVLNode[T] = {
    val rhs = node.rhs
    val rightLhs = rhs.lhs

    node.rhs = rightLhs
    rhs.lhs = node

    node.height = AugmentedAVLTree.computeHeight(node)
    rhs.height = AugmentedAVLTree.computeHeight(rhs)

    rhs
  }

  def rotateRight(node: AugmentedAVLNode[T]): AugmentedAVLNode[T] = {
    val lhs = node.lhs
    val leftRhs = lhs.rhs

    node.lhs = leftRhs
    lhs.rhs = node

    node.height = AugmentedAVLTree.computeHeight(node)
    lhs.height = AugmentedAVLTree.computeHeight(lhs)

    lhs
  }

  def find(value: T):  AugmentedAVLNode[T] = find(root, value)
  def find(node: AugmentedAVLNode[T], value: T):  AugmentedAVLNode[T] = {
    if (node == null) null
    else {
      val res = ord.compare(value, node.value)
      if (res < 0) find(node.lhs, value)
      else if (res == 0) node
      else find(node.rhs, value)
    }
  }
}

object AugmentedAVLTree {

  def computeHeight[T](node: AugmentedAVLNode[T]): Int = {
    def getSetHeight(n: AugmentedAVLNode[T]): Int = if (n == null) 0 else n.height
    if (node == null) 0
    else 1 + Math.max(getSetHeight(node.lhs), getSetHeight(node.rhs))
  }

  def balanceFactor[T](node: AugmentedAVLNode[T]): Int = {
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


class AugmentedAVLNode[T](var value: T, op: (T, T) => T) {

  var _parent: AugmentedAVLNode[T] = _
  private var _lhs: AugmentedAVLNode[T] = _
  private var _rhs: AugmentedAVLNode[T] = _

  def parent: AugmentedAVLNode[T] = _parent
  def lhs: AugmentedAVLNode[T] = _lhs
  def rhs: AugmentedAVLNode[T] = _rhs

  def parent_=(node: AugmentedAVLNode[T]): Unit = {
    _parent = node
  }

  def lhs_=(node: AugmentedAVLNode[T]): Unit = {
    if (_lhs ==  null ||  _lhs != node) {
      _lhs = node
      if (_lhs  != null)
        _lhs.parent = this
      lhsChanged(node)
    }
  }

  def rhs_=(node: AugmentedAVLNode[T]): Unit = {
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

  def lhsChanged(newLhs: AugmentedAVLNode[T]): Unit = recompute(newLhs, rhs)
  def rhsChanged(newRhs: AugmentedAVLNode[T]): Unit = recompute(lhs, newRhs)

  def recompute(lhs: AugmentedAVLNode[T], rhs: AugmentedAVLNode[T]): Unit = {
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

object AugmentedAVLNode {
  def apply[T](value: T, op: (T, T) => T): AugmentedAVLNode[T] = new AugmentedAVLNode(value, op)
}
