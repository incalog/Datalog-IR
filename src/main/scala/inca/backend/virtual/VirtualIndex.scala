package inca.backend.virtual

import org.eclipse.viatra.query.runtime.matchers.context.{IInputKey, IQueryRuntimeContextListener}
import org.eclipse.viatra.query.runtime.matchers.tuple.{ITuple, Tuple, TupleMask}

trait VirtualKey extends IInputKey {
  def getUniqueID: String
  override def getPrettyPrintableName: String = getUniqueID
  override def getStringID: String = getUniqueID
}

// TODO document
trait VirtualIndex {
  var isDirty: Boolean

  def isSupported(key: IInputKey): Boolean

  def processChange(change: truechange.Change): Unit

  def countTuples(mask: TupleMask, seed: ITuple): Int
  def enumerateTuples(mask: TupleMask, seed: ITuple): Iterable[Tuple]
  def enumerateValues(mask: TupleMask, seed: ITuple): Iterable[_]
  def containsTuple(tuple: ITuple): Boolean

  def addListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit
  def removeListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit
}
