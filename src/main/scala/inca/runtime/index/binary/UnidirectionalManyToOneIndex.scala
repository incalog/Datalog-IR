package inca.runtime.index.binary

import inca.runtime.index.IndexKey

import scala.collection.mutable

/*
 * In a UnidirectionalManyToOneIndex, each key uniquely identifies the correponding value, but not vice versa.
 */
class UnidirectionalManyToOneIndex[K,V](val key: IndexKey[_]) extends BinaryIndex[K,V] {
  protected val index: mutable.Map[K, V] = mutable.Map()

  override def entries: Iterable[(K, V)] = index
  override def index(k: K): Iterable[V] = index.get(k)
  override def indexInverted(v: V): Seq[K] = index.toSeq.flatMap(kv => if (kv._2 == v) Some(kv._1) else None)

  override def insert(k: K, v: V): Unit = {
    index += (k -> v)
    notify(k, v, isInsertion = true)
  }

  override def delete(k: K, v: V): Unit = {
    index - k
    notify(k, v, isInsertion = false)
  }

}