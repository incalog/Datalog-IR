package inca.runtime.index.dynamic

import inca.runtime.Database
import inca.runtime.index.DynamicKey
import inca.runtime.index.binary.BidirectionalManyToOneIndex
import truechange._

object ParentIndex {
  case object Key extends DynamicKey {
    override val getStringID: String = "#parent"
    override val getArity: Int = 2
    override def isEnumerable: Boolean = true
  }

  object Factory extends DynamicIndexFactory {
    override def makeIndex(database: Database): DynamicIndex = {
      val ix = new ParentIndex
      ix.setDatabase(database)
      ix
    }
  }
}

class ParentIndex extends BidirectionalManyToOneIndex[URI,URI](ParentIndex.Key)
  with DynamicIndex {

  /** processes edit to update this index accordingly */
  override def processEdit(edit: truechange.Edit): Unit = edit match {
    case DetachUnload(node, tag, kids, lits, link, parent, ptag) =>
      processEdit(Detach(node, tag, link, parent, ptag))
      processEdit(Unload(node, tag, kids, lits))

    case LoadAttach(node, tag, kids, lits, link, parent, ptag) =>
      processEdit(Load(node, tag, kids, lits))
      processEdit(Attach(node, tag, link, parent, ptag))

    case Update(_, _, _, _) =>
      // nothing

    case truechange.Attach(node, _, link, parent, _) => link.getRawLink match {
      case _: ListFirstLink =>
        database.iterateNext(node)(insert(_, parent))
      case _: ListNextLink =>
        Option(index.get(parent)) match {
          case Some(containingList) => database.iterateNext(node)(insert(_, containingList))
          case None =>
        }
      case RootLink => // skip
      case _: NamedLink => insert(node, parent)
    }
    case truechange.Detach(node, _, link, parent, _) => link.getRawLink match {
      case _: ListFirstLink =>
        database.iterateNext(node)(delete(_, parent))
      case _: ListNextLink =>
        Option(index.get(parent)) match {
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
