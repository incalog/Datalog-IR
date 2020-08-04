package inca.runtime.index.binary

import inca.runtime.index.IndexKey

import scala.collection.mutable

/*
 * In a BidirectionalOneToOneIndex, each key uniquely identifies the correponding value and vice versa.
 */
class BidirectionalOneToOneIndex[K,V](val key: IndexKey[_]) extends BinaryIndex[K,V] {
  private[inca] val index: mutable.Map[K, V] = mutable.Map()
  private[inca] val indexInverted: mutable.Map[V, K] = mutable.Map()

  override def entries: Iterable[(K, V)] = index
  override def index(k: K): Iterable[V] = index.get(k)
  override def indexInverted(v: V): Iterable[K] = indexInverted.get(v)

  override def insert(k: K, v: V): Unit = {
    index += (k -> v)
    indexInverted += (v -> k)
    notify(k, v, isInsertion = true)
  }

  override def delete(k: K, v: V): Unit = {
    index -= k
    indexInverted -= v
    notify(k, v, isInsertion = false)
  }

}