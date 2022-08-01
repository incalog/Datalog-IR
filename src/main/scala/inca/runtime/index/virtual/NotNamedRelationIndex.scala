package inca.runtime.index.virtual

import inca.runtime.db.Database
import inca.runtime.index.{BagIndex, Index, IndexKey, NamedRelationKey, VirtualKey}
import inca.util.TupleOps
import org.eclipse.collections.api.factory.Maps
import org.eclipse.collections.api.map.MutableMap
import org.eclipse.viatra.query.runtime.matchers.context.{IInputKey, IQueryRuntimeContextListener}
import org.eclipse.viatra.query.runtime.matchers.tuple.{ITuple, Tuple, TupleMask, TupleMask0, Tuples}
import truechange.URI

import java.util.function.IntSupplier
import scala.collection.mutable


object NotNamedRelationIndex {
  case class Key(namedRelKey: NamedRelationKey, columns: List[IndexKey[_]])
    extends VirtualKey {
    override val getStringID: String =
      s"NotNamedRelationIndex(${namedRelKey.getStringID})"
    override val getArity: Int = columns.size
    override def isEnumerable: Boolean = true
    override def factory: VirtualIndexFactory = NotNamedRelationIndex.Factory
  }

  object Factory extends VirtualIndexFactory {
    override def makeIndex(key: VirtualKey, database: Database): VirtualIndex = key match {
      case NotNamedRelationIndex.Key(namedRelKey, columns) =>
        val ix = new NotNamedRelationIndex(namedRelKey, columns)
        ix.setDatabase(database)
        ix
      case _ =>
        throw new IllegalArgumentException(s"Cannot create index for $key")
    }
  }
}

class NotNamedRelationIndex(namedRelKey: NamedRelationKey, columns: List[IndexKey[_]])
    extends VirtualIndex {

  if (namedRelKey.arity != columns.size)
    throw new IllegalArgumentException(s"Named relation $namedRelKey key must have columns $columns")
  columns.zipWithIndex.foreach { case (k, ix) =>
    if (k.getArity != 1)
      throw new IllegalArgumentException(s"Column $ix of named relation $namedRelKey must have unary arity but was $k")
  }

  /** The key of this index */
  override val key: IndexKey[_] = NotNamedRelationIndex.Key(namedRelKey, columns)

  lazy val namedRelIndex: BagIndex = database.namedRelationInstancesEnsure(namedRelKey.name, namedRelKey.arity)
  lazy val columnIndices: List[Index] = columns.map(database.ensureIndex)

  override def containsTuple(tuple: ITuple): Boolean =
    !namedRelIndex.containsTuple(tuple)

  override def countTuples(mask: TupleMask, seed: ITuple): Int =
    enumerateTuples(mask, seed).size

  override def enumerateTuples(mask: TupleMask, seed: ITuple): Iterable[Tuple] =  {
    if (mask.indices.length == columns.size) {
      val tuple = mask.transform(seed)
      if (namedRelIndex.containsTuple(tuple))
        Iterable.empty
      else
        Iterable.single(tuple)
    } else {
      val seedTuple = mask.revertFrom(seed)
      val columnTuples = columnIndices.zipWithIndex.map { case (colIndex, i) =>
        Option(seedTuple.get(i)) match {
          case None =>
            colIndex
              .enumerateTuples(TupleMask.empty(1), Tuples.staticArityFlatTupleOf())
              .view
              .map(_.get(0))  // get value of singleton tuple
              .toSeq
          case Some(v) =>
            Seq(v)
        }
      }
      val namedRelationCandidates = TupleOps.cartesianProduct(columnTuples).map(Tuples.flatTupleOf(_:_*))
      namedRelationCandidates.filter(t => !namedRelIndex.containsTuple(t))
    }
  }

  override def enumerateValues(mask: TupleMask, seed: ITuple): Iterable[_] = {
    val omittedIx = mask.getFirstOmittedIndex.orElseThrow()
    enumerateTuples(mask, seed).map(_.get(omittedIx))
  }


  case class NegatedListener(listener: IQueryRuntimeContextListener) extends IQueryRuntimeContextListener {
    override def update(key: IInputKey, updateTuple: Tuple, isInsertion: Boolean): Unit =
      listener.update(key, updateTuple, !isInsertion)
  }

  override def addListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit = {
    namedRelIndex.addListener(NegatedListener(listener), seed)
  }

  override def removeListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit =
    namedRelIndex.removeListener(NegatedListener(listener), seed)

  override def afterInitialization(): Unit = {
    // nothing
  }
}
