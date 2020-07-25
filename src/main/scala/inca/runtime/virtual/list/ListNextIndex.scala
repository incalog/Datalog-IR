package inca.runtime.virtual.list

import inca.runtime.index.DynamicKey
import inca.runtime.index.binary.BidirectionalOneToOneIndex
import truechange._

case object ListNextKey extends DynamicKey {
  override val id: String = "#next"
  override val arity: Int = 2
}

class ListNextIndex extends BidirectionalOneToOneIndex[NodeURI, NodeURI] {
  override def key: DynamicKey = ListNextKey

  def iterateNext(from: truechange.NodeURI)(f: truechange.NodeURI => Unit): Unit = {
    f(from)
    var nextNode = index.get(from)
    while(true) {
      nextNode match {
        case Some(node) =>
          f(node)
          nextNode = index.get(node)
        case None => return
      }
    }
  }

  override def processChange(change: truechange.Change): Unit = change match {
    case truechange.Attach(parent, _, link, node, _) => link match {
      case _: ListNextLink => insert(parent, node)
      case _: ListFirstLink => // nothing to do
      case _: NamedLink => // nothing to do
    }
    case truechange.Detach(parent, _, link, node, _) => link match {
      case _: ListNextLink => delete(parent, node)
      case _: ListFirstLink => // nothing to do
      case _: NamedLink => // nothing to do
    }
    case truechange.Load(_, _, _, _) => // nothing to do
    case truechange.Unload(_, _, _, _) => // nothing to do
  }
}
