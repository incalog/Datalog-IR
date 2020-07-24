package inca.runtime.index.binary

import inca.runtime.virtual.VirtualIndex
import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener
import org.eclipse.viatra.query.runtime.matchers.tuple.{Tuple, Tuples}

import scala.collection.mutable

abstract class AbstractBinaryIndex[K,V] extends VirtualIndex {
  protected def insert(k: K, v: V): Unit
  protected def delete(k: K, v: V): Unit

  def hasListener: Boolean = listenAll.nonEmpty || listenKey.nonEmpty || listenVal.nonEmpty || listenKeyVal.nonEmpty
  protected val listenAll: mutable.Set[IQueryRuntimeContextListener] = mutable.Set()
  protected val listenKey: mutable.MultiDict[K, IQueryRuntimeContextListener] = mutable.MultiDict()
  protected val listenVal: mutable.MultiDict[V, IQueryRuntimeContextListener] = mutable.MultiDict()
  protected val listenKeyVal: mutable.MultiDict[Tuple, IQueryRuntimeContextListener] = mutable.MultiDict()

  final protected def notify(k: K, v: V, isInsertion: Boolean): Unit = {
    isDirty |= this.hasListener
    val t = Tuples.staticArityFlatTupleOf(k, v)
    val notify = (listener: IQueryRuntimeContextListener) => listener.update(virtualKey, t, isInsertion)
    listenAll.foreach(notify)
    listenKey.get(k).foreach(notify)
    listenVal.get(v).foreach(notify)
    listenKeyVal.get(t).foreach(notify)
  }

  final override def addListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit = {
    if (seed == null) {
      listenAll += listener
    } else {
      val k = seed.get(0).asInstanceOf[K]
      val v = seed.get(1).asInstanceOf[V]
      if (k == null && v == null) {
        listenAll += listener
      } else if (k == null && v != null) {
        listenVal += (v -> listener)
      } else if (k != null && v == null) {
        listenKey += (k -> listener)
      } else {
        listenKeyVal += (seed -> listener)
      }
    }
  }

  final override def removeListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit = {
    if (seed == null) {
      listenAll -= listener
    } else {
      val k = seed.get(0).asInstanceOf[K]
      val v = seed.get(1).asInstanceOf[V]
      if (k == null && v == null) {
        listenAll -= listener
      } else if (k == null && v != null) {
        listenVal -= (v -> listener)
      } else if (k != null && v == null) {
        listenKey -= (k -> listener)
      } else {
        listenKeyVal -= (seed -> listener)
      }
    }
  }
}
