package inca.runtime.index.dynamic

import inca.runtime.db.Database
import inca.runtime.index.binary.BidirectionalManyToOneIndex
import inca.runtime.index.DynamicKey
import org.eclipse.collections.api.factory.Maps
import org.eclipse.collections.api.map.MutableMap
import scala.jdk.CollectionConverters._
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

  sealed trait ParentDeletion
  case class ParentNodeDeletion(kid: URI, parent: URI) extends ParentDeletion
//  case class
}

class ParentIndex extends BidirectionalManyToOneIndex[URI, URI](ParentIndex.Key) with DynamicIndex {

  // maps from kid to old parent
  private val deletions: MutableMap[URI, URI] = Maps.mutable.empty()

  private def insertOrUpdate(node: URI, parent: URI): Unit = {
    val oldParent = deletions.remove(node)
    if (oldParent == null)
      insert(node, parent)
    else
      update(node, oldParent, parent)
  }

  override def startProcessEditScript(): Unit =
    if (deletions.notEmpty())
      throw new IllegalStateException(s"Nonempty deletions at start of edit script processing")

  override def endProcessEditScript(): Unit = {
    deletions.entrySet().asScala.foreach { e =>
      delete(e.getKey, e.getValue)
    }
    deletions.clear()
  }

  /** processes edit to update this index accordingly */
  override def processEdit(edit: truechange.CoreEdit): Unit = edit match {
    case Update(_, _, _, _) =>
    // nothing

    case truechange.Attach(node, _, link, parent, _) =>
      link.getRawLink match {
        case _: ListFirstLink =>
          database.iterateNext(node)(insertOrUpdate(_, parent))
        case _: ListNextLink =>
          Option(index.get(parent)) match {
            case Some(containingList) =>
              database.iterateNext(node)(insertOrUpdate(_, containingList))
            case None =>
          }
        case RootLink => // skip
        case _: NamedLink => insertOrUpdate(node, parent)
      }
    case truechange.Detach(node, _, link, parent, _) =>
      link.getRawLink match {
        case _: ListFirstLink =>
          database.iterateNext(node)(deletions.put(_, parent))
        case _: ListNextLink =>
          Option(index.get(parent)) match {
            case Some(containingList) =>
              database.iterateNext(node)(deletions.put(_, containingList))
            case None =>
          }
        case RootLink => // skip
        case _: NamedLink => deletions.put(node, parent)
      }
    case truechange.Load(node, _, kids, _) =>
      kids.foreach { case (_, kid) =>
        insertOrUpdate(kid, node)
      }
    case truechange.Unload(node, _, kids, _) =>
      kids.foreach { case (_, kid) =>
        deletions.put(kid, node)
      }
  }
}
