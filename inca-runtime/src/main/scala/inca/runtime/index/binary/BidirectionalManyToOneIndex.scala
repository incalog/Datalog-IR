package inca.runtime.index.binary

import inca.runtime.index.IndexKey
import org.eclipse.collections.api.factory.Maps
import org.eclipse.collections.api.map.MutableMap
import org.eclipse.collections.api.multimap.set.MutableSetMultimap
import org.eclipse.collections.impl.factory.Multimaps

import scala.jdk.CollectionConverters.*

/*
 * In a BidirectionalManyToOneIndex, each key uniquely identifies the correponding value, but not vice versa.
 */
class BidirectionalManyToOneIndex[K,V](val key: IndexKey[_]) extends BinaryMapIndex[K,V] {
  protected val index: MutableMap[K, V] = Maps.mutable.empty()
  protected val indexInverted: MutableSetMultimap[V, K] = Multimaps.mutable.set.empty()

  override def entries: Iterable[(K, V)] = index.entrySet().asScala.map(e => (e.getKey, e.getValue))
  def entrySets: Iterable[(V, Iterable[K])] = indexInverted.keyMultiValuePairsView.asScala.map(p => p.getOne -> p.getTwo.asScala)
  override def index(k: K): Iterable[V] = Option(index.get(k))
  override def indexInverted(v: V): Iterable[K] = indexInverted.get(v).asScala

  override def insert(k: K, v: V): Unit = {
    index.put(k, v)
    indexInverted.put(v, k)
    notify(k, v, isInsertion = true)
  }

  override def delete(k: K, v: V): Unit = {
    index.remove(k)
    indexInverted.remove(v, k)
    notify(k, v, isInsertion = false)
  }

  override def update(k: K, vold: V, vnew: V): Unit = {
    index.put(k, vnew)
    indexInverted.remove(vold, k)
    indexInverted.put(vnew, k)
    notify(k, vnew, isInsertion = true)
    notify(k, vold, isInsertion = false)
  }

}