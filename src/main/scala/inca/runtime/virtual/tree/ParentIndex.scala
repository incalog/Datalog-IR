package inca.runtime.virtual.tree

import inca.runtime.index.DynamicKey
import inca.runtime.index.binary.BidirectionalManyToOneIndex
import inca.runtime.virtual.list.ListNextIndex
import truechange._

case object ParentKey extends DynamicKey {
  override val id: String = "#parent"
  override val arity: Int = 2
}

class ParentIndex(next: ListNextIndex) extends BidirectionalManyToOneIndex[NodeURI,NodeURI] {
  override def key: DynamicKey = ParentKey

  override def processChange(change: truechange.Change): Unit = change match {
    case truechange.Attach(parent, _, link, node, _) => link match {
      case _: ListFirstLink =>
        next.iterateNext(node)(insert(_, parent))
      case _: ListNextLink =>
        index.get(parent) match {
          case Some(containingList) => next.iterateNext(node)(insert(_, containingList))
          case None =>
        }
      case RootLink => // skip
      case _: NamedLink => insert(node, parent)
    }
    case truechange.Detach(parent, _, link, node, _) => link match {
      case _: ListFirstLink =>
        next.iterateNext(node)(delete(_, parent))
      case _: ListNextLink =>
        index.get(parent) match {
          case Some(containingList) => next.iterateNext(node)(delete(_, containingList))
          case None =>
        }
      case RootLink => // skip
      case _: NamedLink => delete(node, parent)
    }
    case truechange.Load(node, _, kids, _) =>
      kids.foreach { case (_, kid) =>
        insert(kid, node)
      }
    case truechange.Unload(node, _, kids, _) =>
      kids.foreach { case (_, kid) =>
        delete(kid, node)
      }
  }
}
