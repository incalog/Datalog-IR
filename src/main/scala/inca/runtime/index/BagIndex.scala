package inca.runtime.index
import org.eclipse.collections.api.map.primitive.MutableObjectIntMap
import org.eclipse.collections.impl.factory.primitive.ObjectIntMaps
import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener
import org.eclipse.viatra.query.runtime.matchers.tuple.{ITuple, Tuple, TupleMask}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

class BagIndex(val key: IndexKey[_]) extends Index {

  protected val index: MutableObjectIntMap[Tuple] = ObjectIntMaps.mutable.empty()

  def insert(t: Tuple): Unit = {
    val old = index.get(t)
    if (old == 0) {
      index.put(t, 1)
      notify(t, isInsertion = true)
    } else {
      index.put(t, old + 1)
    }
  }

  def delete(t: Tuple): Unit = {
    val old = index.getIfAbsent(t, -1)
    if (old == 1) {
      index.remove(t)
      notify(t, isInsertion = false)
    } else {
      index.put(t, old - 1)
    }
  }

  /** checks whether the provided tuple is contained in index */
  final override def containsTuple(tuple: ITuple): Boolean = {
    if (tuple == null)
      return false

    index.containsKey(tuple)
  }

  /** counts tuples of the associated virtual key contained in index based on provided mask and seed */
  override def countTuples(mask: TupleMask, seed: ITuple): Int = {
    val maskLength = mask.indices.length
    if (maskLength == 0) {
      index.keySet().size()
    } else {
      // count tuples that have the elements in seed at position according to mask
      index.keySet().asScala.filter { t =>
        seed.getElements.zipWithIndex.forall { case (c, ix) =>
          c == t.get(ix)
        }
      }.map(index.get).sum
    }
  }

  /** returns all tuples maintained in index associated with virtual key based on provided mask and seed */
  override def enumerateTuples(mask: TupleMask, seed: ITuple): Iterable[Tuple] = {
    val maskLength = mask.indices.length
    if (maskLength ==  0) {
      index.keySet().asScala
    } else {
      // enumerate tuples that have the elements in seed at position according to mask
      index.keySet().asScala.filter { t =>
        seed.getElements.zipWithIndex.forall { case (seedVal, seedIx) =>
          seedVal == t.get(mask.indices(seedIx))
        }
      }
    }
  }

  /** enumerate all values within index associated with virtual key based on provided mask and seed */
  override def enumerateValues(mask: TupleMask, seed: ITuple): Iterable[_] = {
    val maskLength = mask.indices.length
    if (maskLength == key.getArity - 1) {
      index.keySet().asScala.flatMap { t =>
        if (seed.getElements.zipWithIndex.forall { case (seedVal, seedIx) => seedVal == t.get(mask.indices(seedIx)) })
          Some(t.get(mask.getFirstOmittedIndex.getAsInt))
        else
          None
      }
    } else {
      throw new IllegalArgumentException("Invalid tuple mask " + mask + " for enumerateValues in bag index with arity " + key.getArity + " in " + this)
    }
  }

  val listenAll: mutable.Set[IQueryRuntimeContextListener] = mutable.Set()

  final protected def notify(t: Tuple, isInsertion: Boolean): Unit = {
    val notify = (listener: IQueryRuntimeContextListener) => listener.update(key, t, isInsertion)
    listenAll.foreach(notify)
  }


  /** Adds a listener for changes to this index */
  override def addListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit = {
    listenAll += listener
  }

  /** Removes a listener for changes to this index */
  override def removeListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit = {
    listenAll -= listener
  }
}
