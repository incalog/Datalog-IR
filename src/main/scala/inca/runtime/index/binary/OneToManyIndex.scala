package inca.runtime.index.binary

import inca.util.TupleOps
import org.eclipse.viatra.query.runtime.matchers.tuple.{ITuple, Tuple, TupleMask, Tuples}

import scala.collection.mutable

/*
 * In a BinaryInjectiveVirtualIndex, each value uniquely identifies the correponding key, but not and vice versa.
 * One to many.
 */
abstract class OneToManyIndex[K,V] extends AbstractBinaryIndex[K,V] {
  protected val index: mutable.MultiDict[K, V] = mutable.MultiDict()
  protected val indexInverted: mutable.Map[V, K] = mutable.Map()

  override protected def insert(k: K, v: V): Unit = {
    index += (k -> v)
    indexInverted += (v -> k)
    notify(k, v, isInsertion = true)
  }

  override protected def delete(k: K, v: V): Unit = {
    index -= (k -> v)
    indexInverted -= v
    notify(k, v, isInsertion = false)
  }


  final override def containsTuple(tuple: ITuple): Boolean = {
    if (tuple == null)
      return false

    val k = tuple.get(0).asInstanceOf[K]
    val v = tuple.get(1).asInstanceOf[V]
    index.get(k).contains(v)
  }

  final override def countTuples(mask: TupleMask, seed: ITuple): Int = {
    val maskLength = mask.indices.length
    if (maskLength == 0) {
      index.size
    } else if (maskLength == 1) {
      val isOrdered = mask.indices(0) == 0
      if (isOrdered) {
        index.get(seed.get(0).asInstanceOf[K]).size
      } else if (!isOrdered && indexInverted.contains(seed.get(1).asInstanceOf[V])) {
        1
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
      throw new IllegalArgumentException("Invalid tuple mask " + mask + " for bijective virtual index " + this)
    }
  }

  final override def enumerateTuples(mask: TupleMask, seed: ITuple): Iterable[Tuple] = {
    val maskLength = mask.indices.length
    if (maskLength == 0) {
      indexInverted.map { case (v, k) => Tuples.staticArityFlatTupleOf(k, v) }
    } else if (maskLength == 1) {
      val isOrdered = mask.indices(0) == 0
      if (isOrdered) {
        val k = seed.get(0).asInstanceOf[K]
        index.get(k).map(Tuples.staticArityFlatTupleOf(k, _))
      } else {
        val v = seed.get(1).asInstanceOf[V]
        indexInverted.get(v).map(Tuples.staticArityFlatTupleOf(_, v))
      }
    } else if (maskLength == 2) {
      val isOrdered = mask.indices(0) == 0
      if (isOrdered && containsTuple(seed)) {
        Seq(TupleOps.binaryTuple(seed))
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
      val isOrdered = mask.indices(0) == 0
      if (isOrdered) {
        val k = seed.get(0).asInstanceOf[K]
        index.get(k).map(Tuples.staticArityFlatTupleOf(k, _))
      } else {
        val v = seed.get(1).asInstanceOf[V]
        indexInverted.get(v).map(Tuples.staticArityFlatTupleOf(_, v))
      }
    } else {
      throw new IllegalArgumentException("Invalid tuple mask " + mask + " for enumerateValues in bijective virtual index " + this)
    }
  }

}