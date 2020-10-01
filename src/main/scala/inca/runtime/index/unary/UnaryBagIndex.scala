package inca.runtime.index.unary

import inca.runtime.index.IndexKey
import org.eclipse.collections.api.map.primitive.MutableObjectIntMap
import org.eclipse.collections.impl.factory.primitive.ObjectIntMaps

import scala.jdk.CollectionConverters._

class UnaryBagIndex[V](val key: IndexKey[_]) extends UnaryIndex[V] {
//  protected val index: mutable.Map[V, Int] = mutable.Map()
  protected val index: MutableObjectIntMap[V] = ObjectIntMaps.mutable.empty()

  override def entries: Iterable[V] = index.keySet().asScala
  override def index(v: V): Int = Option(index.get(v)).getOrElse(0)

  def insert(v: V): Unit = {
    index.put(v, index.getIfAbsent(v, -1) + 1)
//    var changed = false
//    index.updateWith(v) {
//      case None =>
//        changed = true
//        Some(1)
//      case Some(n) =>
//        Some(n+1)
//    }
//    if (changed)
//      notify(v, isInsertion = true)
  }

  def delete(v: V): Unit = {
//    var changed = false
//    index.updateWith(v) {
//      case None => None
//      case Some(n) =>
//        if (n == 1) {
//          changed = true
//          None
//        } else {
//          Some(n-1)
//        }
//    }
//    if (changed)
//      notify(v, isInsertion = false)
  }

}
