package inca.runtime.index.binary

import inca.runtime.index.IndexKey

import scala.collection.mutable

/*
 * In a BidirectionalOneToManyIndex, each value uniquely identifies the correponding key, but not vice versa.
 */
class BidirectionalOneToManyIndex[K,V](val key: IndexKey[_]) extends BinaryIndex[K,V] {
  protected val index: mutable.MultiDict[K, V] = mutable.MultiDict()
  protected val indexInverted: mutable.Map[V, K] = mutable.Map()

  override def entries: Iterable[(K, V)] = index
  def entrySets: Iterable[(K, Iterable[V])] = index.sets
  override def index(k: K): collection.Set[V] = index.get(k)
  override def indexInverted(v: V): Iterable[K] = indexInverted.get(v)

  override def insert(k: K, v: V): Unit = {
    index += (k -> v)
    indexInverted += (v -> k)
    notify(k, v, isInsertion = true)
  }

  override def delete(k: K, v: V): Unit = {
    index -= (k -> v)
    indexInverted -= v
    notify(k, v, isInsertion = false)
  }

}