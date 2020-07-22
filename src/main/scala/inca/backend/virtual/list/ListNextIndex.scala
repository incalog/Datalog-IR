package inca.backend.virtual.list

import inca.backend.virtual.{BinaryBijectiveVirtualIndex, VirtualKey}
import truechange._

case object ListNextKey extends VirtualKey {
  override val getUniqueID: String = "next"
  override val getArity: Int = 2
  override val isEnumerable: Boolean = true
}

class ListNextIndex extends BinaryBijectiveVirtualIndex[NodeURI, NodeURI] {
  override val virtualKey: VirtualKey = ListNextKey

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
