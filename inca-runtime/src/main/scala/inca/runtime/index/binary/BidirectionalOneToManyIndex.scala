package inca.runtime.index.binary

import inca.runtime.index.IndexKey
import org.eclipse.collections.api.factory.Maps
import org.eclipse.collections.api.map.MutableMap
import org.eclipse.collections.api.multimap.set.MutableSetMultimap
import org.eclipse.collections.impl.factory.Multimaps

import scala.jdk.CollectionConverters.*

/*
 * In a BidirectionalOneToManyIndex, each value uniquely identifies the correponding key, but not vice versa.
 */
class BidirectionalOneToManyIndex[K,V](val key: IndexKey[_]) extends BinaryIndex[K,V] {
  protected val index: MutableSetMultimap[K, V] = Multimaps.mutable.set.empty()
  protected val indexInverted: MutableMap[V, K] = Maps.mutable.empty()

  override def entries: Iterable[(K, V)] = indexInverted.entrySet().asScala.map(e => (e.getValue, e.getKey))
  def entrySets: Iterable[(K, Iterable[V])] = index.keyMultiValuePairsView.asScala.map(p => p.getOne -> p.getTwo.asScala)
  override def index(k: K): Iterable[V] = index.get(k).asScala
  override def indexInverted(v: V): Iterable[K] = Option(indexInverted.get(v))

  override def insert(k: K, v: V): Unit = {
    index.put(k, v)
    indexInverted.put(v, k)
    notify(k, v, isInsertion = true)
  }

  override def delete(k: K, v: V): Unit = {
    index.remove(k, v)
    indexInverted.remove(v)
    notify(k, v, isInsertion = false)
  }

  override def update(k: K, vold: V, vnew: V): Unit = {
    index.remove(k, vold)
    index.put(k, vnew)
    indexInverted.remove(vold)
    indexInverted.put(vnew, k)
    notify(k, vnew, isInsertion = true)
    notify(k, vold, isInsertion = false)
  }
}