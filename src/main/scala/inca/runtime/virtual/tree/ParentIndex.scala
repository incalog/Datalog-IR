package inca.runtime.virtual.tree

import inca.runtime.index.binary.ManyToOneIndex
import inca.runtime.virtual.VirtualKey
import inca.runtime.virtual.list.ListNextIndex
import truechange._

case object ParentKey extends VirtualKey {
  override val getUniqueID: String = "parent"
  override val getArity: Int = 2
  override val isEnumerable: Boolean = true
}

class ParentIndex(next: ListNextIndex) extends ManyToOneIndex[NodeURI,NodeURI] {
  override val virtualKey: VirtualKey = ParentKey

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
