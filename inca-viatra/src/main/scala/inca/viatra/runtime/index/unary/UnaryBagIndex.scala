package inca.viatra.runtime.index.unary

import inca.viatra.runtime.index.IndexKey
import org.eclipse.collections.api.map.primitive.MutableObjectIntMap
import org.eclipse.collections.impl.factory.primitive.ObjectIntMaps

import scala.jdk.CollectionConverters.*

class UnaryBagIndex[V](val key: IndexKey[_]) extends UnaryIndex[V] {
  protected val index: MutableObjectIntMap[V] = ObjectIntMaps.mutable.empty()

  override def entries: Iterable[V] = index.keySet().asScala

  override def index(v: V): Int = Option(index.get(v)).getOrElse(0)

  override def insert(v: V): Unit = {
    val old = index.get(v)
    if (old == 0) {
      index.put(v, 1)
      notify(v, isInsertion = true)
    } else {
      index.put(v, old + 1)
    }
  }

  override def delete(v: V): Unit = {
    val old = index.getIfAbsent(v, -1)
    if (old == 1) {
      index.remove(v)
      notify(v, isInsertion = false)
    } else {
      index.put(v, old - 1)
    }
  }

}
