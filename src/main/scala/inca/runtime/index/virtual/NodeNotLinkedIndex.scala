package inca.runtime.index.virtual

import inca.runtime.db.Database
import inca.runtime.index.binary.BinaryIndex
import inca.runtime.index.dynamic.ParentIndex
import inca.runtime.index.unary.UnaryIndex
import inca.runtime.index.IndexKey
import inca.runtime.index.VirtualKey
import org.eclipse.viatra.query.runtime.matchers.context.IInputKey
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple
import truechange.URI

object NodeNotLinkedIndex {
  case class Key(nodeKey: IndexKey[_], linkKey: IndexKey[_], nodeIsSource: Boolean)
      extends VirtualKey {
    override val getStringID: String =
      s"NodeNotLinked(${nodeKey.getStringID}, ${linkKey.getStringID}, nodeIsSource=$nodeIsSource)"
    override val getArity: Int = 1
    override def isEnumerable: Boolean = true
    override def factory: VirtualIndexFactory = NodeNotLinkedIndex.Factory
  }

  object Factory extends VirtualIndexFactory {
    override def makeIndex(key: VirtualKey, database: Database): VirtualIndex = key match {
      case NodeNotLinkedIndex.Key(nodeKey, linkKey, nodeIsSource) =>
        val ix = new NodeNotLinkedIndex(nodeKey, linkKey, nodeIsSource)
        ix.setDatabase(database)
        ix
      case _ =>
        throw new IllegalArgumentException(s"Cannot create index for $key")
    }
  }
}

class NodeNotLinkedIndex(nodeKey: IndexKey[_], linkKey: IndexKey[_], nodeIsSource: Boolean)
    extends VirtualUnaryIndex[URI] {

  if (nodeKey.getArity != 1)
    throw new IllegalArgumentException(s"Node key must have arity 1")
  if (linkKey.getArity != 2)
    throw new IllegalArgumentException(s"Link key must have arity 2")

  /** The key of this index */
  override val key: IndexKey[_] = NodeNotLinkedIndex.Key(nodeKey, linkKey, nodeIsSource)

  lazy val parentIndex: ParentIndex = database.dynamicIndices.getOrElse(
    ParentIndex.Key,
    throw new IllegalStateException("Size index requires parent index to be present")
  ).asInstanceOf[ParentIndex]

  lazy val nodeIndex: UnaryIndex[URI] = database.getIndex(nodeKey).get.asInstanceOf[UnaryIndex[URI]]
  lazy val linkIndex: URI => Iterable[URI] = {
    val bix = database.getIndex(linkKey).get.asInstanceOf[BinaryIndex[URI, URI]]
    if (nodeIsSource)
      bix.index
    else
      bix.indexInverted
  }

  /** retain those nodes for which no link is defined */
  override def entries: Iterable[URI] =
    nodeIndex.entries.filter(node => linkIndex(node).isEmpty)

  override def index(v: URI): Int =
    if (nodeIndex.index(v) != 0 && linkIndex(v).isEmpty)
      1
    else
      0

  override def afterInitialization(): Unit = {
    // emit node when it is loaded/unloaded, since node.link undef must be true at that time
    database.addUpdateListener(
      nodeKey,
      null,
      (_: IInputKey, updateTuple: Tuple, isInsertion: Boolean) => {
        val node = updateTuple.get(0).asInstanceOf[URI]
        notify(node, isInsertion)
      }
    )

    // emit node when node.link is set/unset the first/last time. Inserting node.link triggers a remove from undef(node.link).
    database.addUpdateListener(
      linkKey,
      null,
      (_: IInputKey, updateTuple: Tuple, isInsertion: Boolean) => {
        val node = updateTuple.get(if (nodeIsSource) 0 else 1).asInstanceOf[URI]

        // if node belongs to nodeIndex
        if (nodeIndex.index(node) != 0) {
          if (isInsertion && linkIndex(node).size == 1) {
            // first insertion of node.link => remove undef(node.link)
            notify(node, isInsertion = false)
          } else if (!isInsertion && linkIndex(node).isEmpty) {
            // last deletion of node.link => insert undef(node.link)
            notify(node, isInsertion = true)
          }
        }
      }
    )
  }
}
