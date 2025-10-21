package inca.viatra.runtime.index.virtual

import inca.viatra.runtime.db.Database
import inca.viatra.runtime.index.{IndexKey, VirtualKey}
import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener
import org.eclipse.viatra.query.runtime.matchers.tuple
import org.eclipse.viatra.query.runtime.matchers.tuple.{ITuple, TupleMask}
import truechange.{Type, URI}

object NotNamedRelationIndex {
  case class Key(name: String, arity: Int) extends VirtualKey {
    override val getStringID: String = s"not#namedRelation#$name($arity)"
    override val getArity: Int = arity

    override def isEnumerable: Boolean = false

    /**
     * We assert statelessness, which is not strictly true. However:
     *   - we only use NotNamedRelationIndex in TypeFilterConstraint
     *   - TypeFilterConstraint defers checking until the tuple is grounded
     *   - the membership of a grounded tuple in NotNamedRelationIndex is stable, because the type of URIs cannot change
     */
    override def isStateless: Boolean = true

    override def factory: VirtualIndexFactory = NotNamedRelationIndex.Factory
  }

  object Factory extends VirtualIndexFactory {
    override def makeIndex(key: VirtualKey, database: Database): VirtualIndex = key match {
      case NotNamedRelationIndex.Key(name, arity) =>
        val ix = new NotNamedRelationIndex(name, arity)
        ix.setDatabase(database)
        ix
      case _ => throw new IllegalArgumentException(s"Cannot create index for $key")
    }
  }
}

class NotNamedRelationIndex(name: String, arity: Int) extends VirtualIndex {

  /** The key of this index */
  override val key: IndexKey[?] = NotNamedRelationIndex.Key(name, arity)

  override def containsTuple(tuple: ITuple): Boolean = database.namedRelationInstances.get(name) match
    case Some(ix) => !ix.containsTuple(tuple)
    case None => true

  override def countTuples(mask: TupleMask, seed: ITuple): Int = throw new UnsupportedOperationException()

  override def enumerateTuples(mask: TupleMask, seed: ITuple): Iterable[tuple.Tuple] = throw new UnsupportedOperationException()

  override def enumerateValues(mask: TupleMask, seed: ITuple): Iterable[?] = throw new UnsupportedOperationException()

  override def addListener(listener: IQueryRuntimeContextListener, seed: tuple.Tuple): Unit = throw new UnsupportedOperationException()

  override def removeListener(listener: IQueryRuntimeContextListener, seed: tuple.Tuple): Unit = throw new UnsupportedOperationException()

  override def afterInitialization(): Unit = {
    // do nothing
  }


}
