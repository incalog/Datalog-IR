package inca.runtime.index.binary

import inca.runtime.index.IndexKey

import scala.collection.mutable

/*
 * In a BidirectionalManyToManyIndex, neither key nor value uniquely identify each other.
 */
class BidirectionalManyToManyIndex[K,V](val key: IndexKey[_]) extends BinaryIndex[K,V] {
  protected val index: mutable.MultiDict[K, V] = mutable.MultiDict()
  protected val indexInverted: mutable.MultiDict[V, K] = mutable.MultiDict()

  override def entries: Iterable[(K, V)] = index
  override def index(k: K): collection.Set[V] = index.get(k)
  override def indexInverted(v: V): collection.Set[K] = indexInverted.get(v)

  override def insert(k: K, v: V): Unit = {
    index += (k -> v)
    indexInverted += (v -> k)
    notify(k, v, isInsertion = true)
  }

  override def delete(k: K, v: V): Unit = {
    index -= (k -> v)
    indexInverted -= (v -> k)
    notify(k, v, isInsertion = false)
  }



}