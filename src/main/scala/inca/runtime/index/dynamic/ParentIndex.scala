package inca.runtime.index.dynamic

import inca.runtime.index.DynamicKey
import inca.runtime.index.binary.BidirectionalManyToOneIndex
import truechange._

object ParentIndex {
  case object Key extends DynamicKey {
    override val getStringID: String = "#parent"
    override val getArity: Int = 2
    override def isEnumerable: Boolean = true
  }

  def apply(): (DynamicKey, ParentIndex) =
    Key -> new ParentIndex()
}

class ParentIndex extends BidirectionalManyToOneIndex[URI,URI](ParentIndex.Key)
  with DynamicIndex {


  override def processEdit(edit: truechange.Edit): Unit = edit match {
    case truechange.Attach(parent, _, link, node, _) => link.getRawLink match {
      case _: ListFirstLink =>
        database.iterateNext(node)(insert(_, parent))
      case _: ListNextLink =>
        index.get(parent) match {
          case Some(containingList) => database.iterateNext(node)(insert(_, containingList))
          case None =>
        }
      case RootLink => // skip
      case _: NamedLink => insert(node, parent)
    }
    case truechange.Detach(parent, _, link, node, _) => link.getRawLink match {
      case _: ListFirstLink =>
        database.iterateNext(node)(delete(_, parent))
      case _: ListNextLink =>
        index.get(parent) match {
          case Some(containingList) => database.iterateNext(node)(delete(_, containingList))
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
