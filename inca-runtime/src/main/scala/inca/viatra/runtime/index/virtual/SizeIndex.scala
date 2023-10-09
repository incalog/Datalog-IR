package inca.viatra.runtime.index.virtual

import inca.viatra.runtime.db.Database
import inca.viatra.runtime.index.{IndexKey, NodeTypeKey, VirtualKey}
import inca.viatra.runtime.index.dynamic.ParentIndex
import org.eclipse.viatra.query.runtime.matchers.context.IInputKey
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple
import truechange.*

object SizeIndex {
  case object Key extends VirtualKey {
    override val getStringID: String = "#size"
    override val getArity: Int = 2
    override def isEnumerable: Boolean = true
    override def factory: VirtualIndexFactory = SizeIndex.Factory
  }

  object Factory extends VirtualIndexFactory {
    override def makeIndex(key: VirtualKey, database: Database): VirtualIndex = key match {
      case SizeIndex.Key =>
        val ix = new SizeIndex
        ix.setDatabase(database)
        ix
      case _ => throw new IllegalArgumentException(s"Cannot create index for $key")
    }
  }
}


class SizeIndex extends VirtualBinaryIndex[URI, Int] {

  /** The key of this index */
  override val key: IndexKey[_] = SizeIndex.Key

  lazy val parentIndex: ParentIndex = database.dynamicIndices.getOrElse(ParentIndex.Key, throw new IllegalStateException("Size index requires parent index to be present")).asInstanceOf[ParentIndex]

  private val anylist = ListType(AnyType)
  private def isList(k: URI): Boolean = database.nodeInstances(anylist).index(k) != 0

  override def entries: Iterable[(URI, Int)] = parentIndex.entrySets.flatMap { case (k,v) =>
    if (isList(k)) {
      Some(k -> v.size)
    } else {
      None
    }
  }

  override def index(k: URI): Iterable[Int] =
    if (isList(k)) {
      Iterable.single(parentIndex.indexInverted(k).size)
    } else {
      Iterable.empty
    }

  override def indexInverted(v: Int): Iterable[URI] = parentIndex.entrySets.flatMap(kv => if (kv._2.size == v) Some(kv._1) else None)

  override def afterInitialization(): Unit = {
    // emit size 0 for loaded/unloaded lists
    database.addUpdateListener(NodeTypeKey(anylist), null, (_: IInputKey, updateTuple: Tuple, isInsertion: Boolean) => {
      val list = updateTuple.get(0).asInstanceOf[URI]
      notify(list, 0, isInsertion)
    })

    // emit a size update when adding/removing children from a list
    database.addUpdateListener(ParentIndex.Key, null, (_: IInputKey, updateTuple: Tuple, isInsertion: Boolean) => {
      val container = updateTuple.get(1).asInstanceOf[URI]
      if (isList(container)) {
        val newsize = parentIndex.indexInverted(container).size
        val oldsize = if (isInsertion) newsize - 1 else newsize + 1
        notify(container, oldsize, isInsertion = false)
        notify(container, newsize, isInsertion = true)
      }
    })
  }
}
