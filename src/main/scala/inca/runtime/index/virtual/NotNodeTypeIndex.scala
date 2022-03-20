package inca.runtime.index.virtual

import inca.runtime.db.Database
import inca.runtime.index.IndexKey
import inca.runtime.index.VirtualKey
import truechange.Type
import truechange.URI

object NotNodeTypeIndex {
  case class Key(id: Type) extends VirtualKey {
    override val getStringID: String = "not#nodeType#" + id.toString
    override val getArity: Int = 1
    override def isEnumerable: Boolean = false

    /**
     * We assert statelessness, which is not strictly true. However:
     *   - we only use NotNodeTypeKey in TypeFilterConstraint
     *   - TypeFilterConstraint defers checking until the tuple is grounded
     *   - the membership of a grounded tuple in NotNodeTypeKey is stable, because the type of URIs
     *     cannot change
     */
    override def isStateless: Boolean = true

    override def factory: VirtualIndexFactory = NotNodeTypeIndex.Factory
  }

  object Factory extends VirtualIndexFactory {
    override def makeIndex(key: VirtualKey, database: Database): VirtualIndex = key match {
      case NotNodeTypeIndex.Key(ty) =>
        val ix = new NotNodeTypeIndex(ty)
        ix.setDatabase(database)
        ix
      case _ => throw new IllegalArgumentException(s"Cannot create index for $key")
    }
  }
}

class NotNodeTypeIndex(ty: Type) extends VirtualUnaryIndex[URI] {

  /** The key of this index */
  override val key: IndexKey[_] = NotNodeTypeIndex.Key(ty)

  override def index(v: URI): Int = database.nodeInstances.get(ty) match {
    case Some(ix) =>
      if (ix.index(v) == 0)
        1
      else
        0
    case None => 1 // tuple does not have type ty
  }

  override def entries: Iterable[URI] = throw new UnsupportedOperationException(
    s"Cannot enumerate nodes _not_ of type $ty"
  )

  override def afterInitialization(): Unit = {
    // do nothing
  }

}
