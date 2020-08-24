package inca.runtime.index.virtual

import inca.runtime.Database
import inca.runtime.index.binary.BinaryIndex
import inca.runtime.index.dynamic.ParentIndex
import inca.runtime.index.{IndexKey, NodeTypeKey, VirtualKey}
import org.eclipse.viatra.query.runtime.matchers.context.IInputKey
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple
import truechange._

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


class SizeIndex extends BinaryIndex[URI, Int]
  with VirtualIndex {

  /** The key of this index */
  override val key: IndexKey[_] = SizeIndex.Key

  lazy val parentIndex: ParentIndex = database.dynamicIndices.getOrElse(ParentIndex.Key, throw new IllegalStateException("Size index requires parent index to be present")).asInstanceOf[ParentIndex]

  override def entries: Iterable[(URI, Int)] = parentIndex.entrySets.map(kv => (kv._1, kv._2.size))
  override def index(k: URI): Iterable[Int] = Iterable.single(parentIndex.indexInverted(k).size)
  override def indexInverted(v: Int): Iterable[URI] = parentIndex.entrySets.flatMap(kv => if (kv._2.size == v) Some(kv._1) else None)

  override def insert(k: URI, v: Int): Unit = throw new UnsupportedOperationException
  override def delete(k: URI, v: Int): Unit = throw new UnsupportedOperationException

  override def afterInitialization(): Unit = {
    val anylist = ListType(AnyType)

    // emit size 0 for loaded/unloaded lists
    database.addUpdateListener(NodeTypeKey(anylist), null, (_: IInputKey, updateTuple: Tuple, isInsertion: Boolean) => {
      val list = updateTuple.get(0).asInstanceOf[URI]
      notify(list, 0, isInsertion)
    })

    // emit a size update when adding/removing children from a list
    database.addUpdateListener(ParentIndex.Key, null, (_: IInputKey, updateTuple: Tuple, isInsertion: Boolean) => {
      val container = updateTuple.get(1).asInstanceOf[URI]
      if (database.nodeInstances(anylist).index(container) != 0) {
        val newsize = parentIndex.indexInverted(container).size
        val oldsize = if (isInsertion) newsize - 1 else newsize + 1
        notify(container, oldsize, isInsertion = false)
        notify(container, newsize, isInsertion = true)
      }
    })
  }
}
