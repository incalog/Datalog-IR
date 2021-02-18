package inca.runtime.index
import org.eclipse.collections.api.map.primitive.MutableObjectIntMap
import org.eclipse.collections.impl.factory.primitive.ObjectIntMaps
import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener
import org.eclipse.viatra.query.runtime.matchers.tuple.{ITuple, Tuple, TupleMask}

class BagIndex(val key: IndexKey[_]) extends Index {

  protected val index: MutableObjectIntMap[Tuple] = ObjectIntMaps.mutable.empty()

  /** counts tuples of the associated virtual key contained in index based on provided mask and seed */
  override def countTuples(mask: TupleMask, seed: ITuple): Int = ???

  /** returns all tuples maintained in index associated with virtual key based on provided mask and seed */
  override def enumerateTuples(mask: TupleMask, seed: ITuple): Iterable[Tuple] = ???

  /** enumerate all values within index associated with virtual key based on provided mask and seed */
  override def enumerateValues(mask: TupleMask, seed: ITuple): Iterable[_] = ???

  /** checks whether the provided tuple is contained in index */
  override def containsTuple(tuple: ITuple): Boolean = ???

  /** Adds a listener for changes to this index */
  override def addListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit = ???

  /** Removes a listener for changes to this index */
  override def removeListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit = ???
}
