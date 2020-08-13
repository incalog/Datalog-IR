package inca.runtime.index.unary

import inca.runtime.index.Index
import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener
import org.eclipse.viatra.query.runtime.matchers.tuple.{ITuple, Tuple, TupleMask, Tuples}

import scala.collection.mutable

abstract class UnaryIndex[V] extends Index {

  def entries: Iterable[V]
  def index(v: V): Int

  final override def containsTuple(tuple: ITuple): Boolean = {
    if (tuple == null)
      return false

    val v = tuple.get(0).asInstanceOf[V]
    index(v) != 0
  }

  final override def countTuples(mask: TupleMask, seed: ITuple): Int = {
    val maskLength = mask.indices.length
    if (maskLength == 0) {
      entries.size
    } else if (maskLength == 1) {
      if (index(seed.get(0).asInstanceOf[V]) != 0) {
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
      entries.map(Tuples.staticArityFlatTupleOf(_))
    } else if (maskLength == 1) {
      val v = seed.get(0).asInstanceOf[V]
      if (index(v) != 0) {
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
      if (index(v) != 0) {
        Seq(Tuples.staticArityFlatTupleOf(v))
      } else {
        Seq()
      }
    } else {
      throw new IllegalArgumentException("Invalid tuple mask " + mask + " for enumerateValues in bijective virtual index " + this)
    }
  }

  protected val listenAll: mutable.Set[IQueryRuntimeContextListener] = mutable.Set()
  protected val listenVal: mutable.MultiDict[V, IQueryRuntimeContextListener] = mutable.MultiDict()

  final protected def notify(v: V, isInsertion: Boolean): Unit = {
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
