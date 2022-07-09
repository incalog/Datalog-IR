package inca.runtime.index.virtual

import inca.runtime.db.Database
import inca.runtime.index.NamedRelationKey
import inca.runtime.index.VirtualKey
import org.eclipse.viatra.query.runtime.matchers.tuple.ITuple

object NotInNamedRelationIndex {
  case class Key(inner: NamedRelationKey) extends VirtualKey {
    override def getStringID: String = s"not#namedRelation#${inner.name}(${inner.arity})"
    override def getArity: Int = inner.arity
    override def isEnumerable: Boolean = false

    /**
     * We assert statelessness, which is not strictly true. However:
     *   - we only use NotInNamedRelationKey in TypeFilterConstraint
     *   - TypeFilterConstraint defers checking until the tuple is grounded
     *   - the membership of a grounded tuple in NotInNamedRelationKey is stable, because named
     *     relations cannot change
     */
    override def isStateless: Boolean = true

    override def factory: VirtualIndexFactory = NotInNamedRelationIndex.Factory
  }

  object Factory extends VirtualIndexFactory {
    override def makeIndex(key: VirtualKey, database: Database): VirtualIndex = key match {
      case nkey @ NotInNamedRelationIndex.Key(_) =>
        val ix = new NotInNamedRelationIndex(nkey)
        ix.setDatabase(database)
        ix
      case _ => throw new IllegalArgumentException(s"Cannot create index for $key")
    }
  }
}
class NotInNamedRelationIndex(key: NotInNamedRelationIndex.Key) extends VirtualBagIndex(key.inner) {

  override def containsTuple(tuple: ITuple): Boolean = {
    if (tuple == null)
      return false

    database.namedRelationInstances.get(key.inner.name) match {
      case Some(index) =>
        val contains = index.containsTuple(tuple)
        !contains
      case None => true
    }
  }

  override def afterInitialization(): Unit = {
    // TODO correct?
    // do nothing
  }
}
