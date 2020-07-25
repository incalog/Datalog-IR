package inca.runtime.index.unary

import inca.runtime.index.VirtualIndex
import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener
import org.eclipse.viatra.query.runtime.matchers.tuple.{ITuple, Tuple, TupleMask, Tuples}

import scala.collection.mutable

abstract class UnaryIndex[V] extends VirtualIndex {
  protected val index: mutable.Set[V] = mutable.Set()

  protected def insert(v: V): Unit = {
    index += v
    notify(v, isInsertion = true)
  }

  protected def delete(v: V): Unit = {
    index -= v
    notify(v, isInsertion = false)
  }


  final override def containsTuple(tuple: ITuple): Boolean = {
    if (tuple == null)
      return false

    val v = tuple.get(0).asInstanceOf[V]
    index.contains(v)
  }

  final override def countTuples(mask: TupleMask, seed: ITuple): Int = {
    val maskLength = mask.indices.length
    if (maskLength == 0) {
      index.size
    } else if (maskLength == 1) {
      if (index.contains(seed.get(0).asInstanceOf[V])) {
        1
      } else {
        0
      }
    } else {
      throw new IllegalArgumentException("Invalid tuple mask " + mask + " for bijective virtual index " + this)
    }
  }

  final override def enumerateTuples(mask: TupleMask, seed: ITuple): Iterable[Tuple] = {
    val maskLength = mask.indices.length
    if (maskLength == 0) {
      index.map(Tuples.staticArityFlatTupleOf(_))
    } else if (maskLength == 1) {
      val v = seed.get(0).asInstanceOf[V]
      if (index.contains(v)) {
        Seq(Tuples.staticArityFlatTupleOf(v))
      } else {
        Seq()
      }
    } else {
      throw new IllegalArgumentException("Invalid tuple mask " + mask + " for bijective virtual index " + this)
    }
  }

  final override def enumerateValues(mask: TupleMask, seed: ITuple): Iterable[Tuple] = {
    val maskLength = mask.indices.length
    if (maskLength == 1) {
      val v = seed.get(0).asInstanceOf[V]
      if (index.contains(v)) {
        Seq(Tuples.staticArityFlatTupleOf(v))
      } else {
        Seq()
      }
    } else {
      throw new IllegalArgumentException("Invalid tuple mask " + mask + " for enumerateValues in bijective virtual index " + this)
    }
  }

  def hasListener: Boolean = listenAll.nonEmpty || listenVal.nonEmpty
  protected val listenAll: mutable.Set[IQueryRuntimeContextListener] = mutable.Set()
  protected val listenVal: mutable.MultiDict[V, IQueryRuntimeContextListener] = mutable.MultiDict()

  final protected def notify(v: V, isInsertion: Boolean): Unit = {
    isDirty |= this.hasListener
    val t = Tuples.staticArityFlatTupleOf(v)
    val notify = (listener: IQueryRuntimeContextListener) => listener.update(key, t, isInsertion)
    listenAll.foreach(notify)
    listenVal.get(v).foreach(notify)
  }

  final override def addListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit = {
    if (seed == null) {
      listenAll += listener
    } else {
      val v = seed.get(0).asInstanceOf[V]
      if (v == null) {
        listenAll += listener
      } else  {
        listenVal += (v -> listener)
      }
    }
  }

  final override def removeListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit = {
    if (seed == null) {
      listenAll -= listener
    } else {
      val v = seed.get(0).asInstanceOf[V]
      if (v == null) {
        listenAll -= listener
      } else if (v != null) {
        listenVal -= (v -> listener)
      }
    }
  }
}
