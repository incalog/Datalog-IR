package inca.runtime.virtual

import org.eclipse.viatra.query.runtime.matchers.context.{IInputKey, IQueryRuntimeContextListener}
import org.eclipse.viatra.query.runtime.matchers.tuple.{ITuple, Tuple, TupleMask}

/**
 * Generic trait representing a virtual key.
 */
trait VirtualKey extends IInputKey {
  def getUniqueID: String
  override def getPrettyPrintableName: String = getUniqueID
  override def getStringID: String = getUniqueID
}

/**
 * Generic trait to capture functionality to support virtual indices.
 * An implementation needs an associated virtual key implementation.
 */
trait VirtualIndex {
  val virtualKey: VirtualKey

  //
  var isDirty: Boolean = false

  // process change and updates index/notifies listeners accordingly
  def processChange(change: truechange.Change): Unit

  // counts tuples of the associated virtual key contained in index based on provided mask and seed
  def countTuples(mask: TupleMask, seed: ITuple): Int
  // returns all tuples maintained in index associated with virtual key based on provided mask and seed
  def enumerateTuples(mask: TupleMask, seed: ITuple): Iterable[Tuple]
  // enumerate all values within index associated with virtual key based on provided mask and seed
  def enumerateValues(mask: TupleMask, seed: ITuple): Iterable[_]
  // checks whether the provided tuple is contained in index
  def containsTuple(tuple: ITuple): Boolean

  def addListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit
  def removeListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit
}


