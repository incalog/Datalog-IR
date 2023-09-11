package inca.runtime.index.binary

import inca.runtime.index.IndexKey
import org.eclipse.collections.api.factory.Maps
import org.eclipse.collections.api.map.MutableMap

import scala.jdk.CollectionConverters.*

/*
 * In a UnidirectionalManyToOneIndex, each key uniquely identifies the correponding value, but not vice versa.
 */
class UnidirectionalManyToOneIndex[K,V](val key: IndexKey[_]) extends BinaryIndex[K,V] {
  protected val index: MutableMap[K, V] = Maps.mutable.empty()

  override def entries: Iterable[(K, V)] = index.entrySet().asScala.map(e => (e.getKey, e.getValue))
  override def index(k: K): Iterable[V] = Option(index.get(k))
  override def indexInverted(v: V): Iterable[K] = index.entrySet().asScala.flatMap(kv => if (kv.getValue == v) Some(kv.getKey) else None)

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