package inca.runtime.index.binary

import inca.runtime.index.IndexKey

import scala.collection.mutable

/*
 * In a BidirectionalManyToOneIndex, each key uniquely identifies the correponding value, but not vice versa.
 */
class BidirectionalManyToOneIndex[K,V](val key: IndexKey[_]) extends BinaryIndex[K,V] {
  protected val index: mutable.Map[K, V] = mutable.Map()
  protected val indexInverted: mutable.MultiDict[V, K] = mutable.MultiDict()

  override def entries: Iterable[(K, V)] = index
  def entrySets: Iterable[(V, Iterable[K])] = indexInverted.sets
  override def index(k: K): Iterable[V] = index.get(k)
  override def indexInverted(v: V): collection.Set[K] = indexInverted.get(v)

  override def insert(k: K, v: V): Unit = {
//    index += (k -> v)
//    indexInverted += (v -> k)
//    notify(k, v, isInsertion = true)
  }

  override def delete(k: K, v: V): Unit = {
    index -= k
    indexInverted -= (v -> k)
    notify(k, v, isInsertion = false)
  }

}