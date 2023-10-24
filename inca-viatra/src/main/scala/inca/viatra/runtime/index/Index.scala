package inca.viatra.runtime.index

import inca.viatra.runtime.index.dynamic.ParentIndex
import inca.viatra.runtime.index.dynamic.ParentIndex.Factory
import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener
import org.eclipse.viatra.query.runtime.matchers.tuple.{ITuple, Tuple, TupleMask}
import truechange.{Link, Tag, URI}


/**
 * Generic trait to capture functionality to support virtual indices.
 * An implementation needs an associated virtual key implementation.
 */
trait Index {
  /** The key of this index */
  def key: IndexKey[_]

  /** counts tuples of the associated virtual key contained in index based on provided mask and seed */
  def countTuples(mask: TupleMask, seed: ITuple): Int
  /** returns all tuples maintained in index associated with virtual key based on provided mask and seed  */
  def enumerateTuples(mask: TupleMask, seed: ITuple): Iterable[Tuple]
  /** enumerate all values within index associated with virtual key based on provided mask and seed */
  def enumerateValues(mask: TupleMask, seed: ITuple): Iterable[_]
  /** checks whether the provided tuple is contained in index */
  def containsTuple(tuple: ITuple): Boolean

  /** Adds a listener for changes to this index */
  def addListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit
  /** Removes a listener for changes to this index */
  def removeListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit
}

object Index {
  val allDynamicIndices = Seq(Factory)
}

case class IndexDeletion(node: URI, tag: Tag, link: Link, parent: URI, ptag: Tag)