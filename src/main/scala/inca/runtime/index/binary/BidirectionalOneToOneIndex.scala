package inca.runtime.index.binary

import inca.runtime.index.IndexKey
import org.eclipse.collections.api.bimap.MutableBiMap
import org.eclipse.collections.api.factory.BiMaps
import scala.jdk.CollectionConverters._

/*
 * In a BidirectionalOneToOneIndex, each key uniquely identifies the correponding value and vice versa.
 */
class BidirectionalOneToOneIndex[K, V](val key: IndexKey[_]) extends BinaryMapIndex[K, V] {
  private[inca] val index: MutableBiMap[K, V] = BiMaps.mutable.empty()

  override def entries: Iterable[(K, V)] = index.entrySet().asScala.map(e => (e.getKey, e.getValue))
  override def index(k: K): Iterable[V] = Option(index.get(k))
  override def indexInverted(v: V): Iterable[K] = Option(index.inverse().get(v))

  override def insert(k: K, v: V): Unit = {
    index.put(k, v)
    notify(k, v, isInsertion = true)
  }

  override def delete(k: K, v: V): Unit = {
    index.remove(k)
    notify(k, v, isInsertion = false)
  }

  override def update(k: K, vold: V, vnew: V): Unit = {
    index.put(k, vnew)
    notify(k, vnew, isInsertion = true)
    notify(k, vold, isInsertion = false)
  }
}
