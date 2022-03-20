package inca.runtime.index.binary

import inca.runtime.index.Index
import inca.util.TupleOps
import org.eclipse.collections.api.multimap.set.MutableSetMultimap
import org.eclipse.collections.impl.factory.Multimaps
import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener
import org.eclipse.viatra.query.runtime.matchers.tuple.ITuple
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple
import org.eclipse.viatra.query.runtime.matchers.tuple.TupleMask
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import scala.collection.mutable
import scala.jdk.FunctionWrappers.AsJavaConsumer

/* Marker trait for binary indices that uniquely identify V from K */
trait BinaryMapIndex[K, V] extends BinaryIndex[K, V]

abstract class BinaryIndex[K, V] extends Index {
  def entries: Iterable[(K, V)]
  def index(k: K): Iterable[V]
  def indexInverted(v: V): Iterable[K]

  final override def containsTuple(tuple: ITuple): Boolean = {
    if (tuple == null)
      return false

    val k = tuple.get(0).asInstanceOf[K]
    val v = tuple.get(1).asInstanceOf[V]
    index(k).exists(_ == v)
  }

  final override def countTuples(mask: TupleMask, seed: ITuple): Int = {
    val maskLength = mask.indices.length
    if (maskLength == 0) {
      entries.size
    } else if (maskLength == 1) {
      val isOrdered = mask.indices(0) == 0
      if (isOrdered) {
        index(seed.get(0).asInstanceOf[K]).size
      } else if (!isOrdered) {
        indexInverted(seed.get(1).asInstanceOf[V]).size
      } else {
        0
      }
    } else if (maskLength == 2) {
      val isOrdered = mask.indices(0) == 0
      if (isOrdered && containsTuple(seed)) {
        1
      } else if (!isOrdered && containsTuple(TupleOps.binaryFlip(seed))) {
        1
      } else {
        0
      }
    } else {
      throw new IllegalArgumentException(
        "Invalid tuple mask " + mask + " for bijective virtual index " + this
      )
    }
  }

  final override def enumerateTuples(mask: TupleMask, seed: ITuple): Iterable[Tuple] = {
    val maskLength = mask.indices.length
    if (maskLength == 0) {
      entries.map { case (k, v) => Tuples.staticArityFlatTupleOf(k, v) }
    } else if (maskLength == 1) {
      val isOrdered = mask.indices(0) == 0
      if (isOrdered) {
        val k = seed.get(0).asInstanceOf[K]
        index(k).map(Tuples.staticArityFlatTupleOf(k, _))
      } else {
        val v = seed.get(1).asInstanceOf[V]
        indexInverted(v).map(Tuples.staticArityFlatTupleOf(_, v))
      }
    } else if (maskLength == 2) {
      val isOrdered = mask.indices(0) == 0
      if (isOrdered && containsTuple(seed)) {
        Seq(TupleOps.binaryTuple(seed))
      } else if (!isOrdered && containsTuple(TupleOps.binaryFlip(seed))) {
        Seq(TupleOps.binaryFlip(seed))
      } else {
        Seq()
      }
    } else {
      throw new IllegalArgumentException(
        "Invalid tuple mask " + mask + " for bijective virtual index " + this
      )
    }
  }

  final override def enumerateValues(mask: TupleMask, seed: ITuple): Iterable[Any] = {
    val maskLength = mask.indices.length
    if (maskLength == 1) {
      val isOrdered = mask.indices(0) == 0
      if (isOrdered) {
        val k = seed.get(0).asInstanceOf[K]
        index(k)
      } else {
        val v = seed.get(1).asInstanceOf[V]
        indexInverted(v)
      }
    } else {
      throw new IllegalArgumentException(
        "Invalid tuple mask " + mask + " for enumerateValues in binary index " + this
      )
    }
  }

  def insert(k: K, v: V): Unit
  def delete(k: K, v: V): Unit
  def update(k: K, vold: V, vnew: V): Unit

  protected val listenAll: mutable.Set[IQueryRuntimeContextListener] = mutable.Set()
  protected val listenKey: MutableSetMultimap[K, IQueryRuntimeContextListener] =
    Multimaps.mutable.set.empty()
  protected val listenVal: MutableSetMultimap[V, IQueryRuntimeContextListener] =
    Multimaps.mutable.set.empty()
  protected val listenKeyVal: MutableSetMultimap[Tuple, IQueryRuntimeContextListener] =
    Multimaps.mutable.set.empty()

  final protected def notify(k: K, v: V, isInsertion: Boolean): Unit = {
    val t = Tuples.staticArityFlatTupleOf(k, v)
    val notify = (listener: IQueryRuntimeContextListener) => listener.update(key, t, isInsertion)
    val notifyConsumer = AsJavaConsumer(notify)
    listenAll.foreach(notify)
    listenKey.get(k).stream().forEach(notifyConsumer)
    listenVal.get(v).stream().forEach(notifyConsumer)
    listenKeyVal.get(t).stream().forEach(notifyConsumer)
  }

  override def addListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit = {
    if (seed == null) {
      listenAll += listener
    } else {
      val k = seed.get(0).asInstanceOf[K]
      val v = seed.get(1).asInstanceOf[V]
      if (k == null && v == null) {
        listenAll += listener
      } else if (k == null && v != null) {
        listenVal.put(v, listener)
      } else if (k != null && v == null) {
        listenKey.put(k, listener)
      } else {
        listenKeyVal.put(seed, listener)
      }
    }
  }

  override def removeListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit = {
    if (seed == null) {
      listenAll -= listener
    } else {
      val k = seed.get(0).asInstanceOf[K]
      val v = seed.get(1).asInstanceOf[V]
      if (k == null && v == null) {
        listenAll -= listener
      } else if (k == null && v != null) {
        listenVal.remove(v, listener)
      } else if (k != null && v == null) {
        listenKey.remove(k, listener)
      } else {
        listenKeyVal.remove(seed, listener)
      }
    }
  }
}
