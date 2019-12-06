package org.inca

trait Node[T] extends T {
  var ancestor: Node[Any]
  //  val `type`: Type = T.
  var leftChild: Node[Any]
  var rightChild: Node[Any]

  def getAncestor[S](concept: Boolean = false): Node[S] = {
    if (concept) {
      if (ancestor.isInstanceOf[S]) {
        return ancestor.asInstanceOf[Node[S]]
      }
    }
    null
  }
}
