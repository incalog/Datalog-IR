package inca.viatra.runtime.index.binary

import inca.viatra.runtime.index.IndexKey
import org.eclipse.collections.api.multimap.set.MutableSetMultimap
import org.eclipse.collections.impl.factory.Multimaps

import scala.jdk.CollectionConverters.*

/*
 * In a BidirectionalManyToManyIndex, neither key nor value uniquely identify each other.
 */
class BidirectionalManyToManyIndex[K, V](val key: IndexKey[_]) extends BinaryIndex[K, V] {
  protected val index: MutableSetMultimap[K, V] = Multimaps.mutable.set.empty()
  protected val indexInverted: MutableSetMultimap[V, K] = Multimaps.mutable.set.empty()

  override def entries: Iterable[(K, V)] = index.keyValuePairsView().asScala.map(p => p.getOne -> p.getTwo)

  override def index(k: K): collection.Set[V] = index.get(k).asScala

  override def indexInverted(v: V): collection.Set[K] = indexInverted.get(v).asScala

  override def insert(k: K, v: V): Unit = {
    index.put(k, v)
    indexInverted.put(v, k)
    notify(k, v, isInsertion = true)
  }

  override def delete(k: K, v: V): Unit = {
    index.remove(k, v)
    indexInverted.remove(v, k)
    notify(k, v, isInsertion = false)
  }

  override def update(k: K, vold: V, vnew: V): Unit = {
    index.remove(k, vold)
    index.put(k, vnew)
    indexInverted.remove(vold, k)
    indexInverted.put(vnew, k)
    notify(k, vnew, isInsertion = true)
    notify(k, vold, isInsertion = false)
  }

}