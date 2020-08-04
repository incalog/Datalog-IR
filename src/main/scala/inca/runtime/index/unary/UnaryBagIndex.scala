package inca.runtime.index.unary

import inca.runtime.index.IndexKey

import scala.collection.mutable

class UnaryBagIndex[V](val key: IndexKey[_]) extends UnaryIndex[V] {
  protected val index: mutable.Map[V, Int] = mutable.Map()

  override def entries: Iterable[V] = index.keys
  override def index(v: V): Int = index.getOrElse(v, 0)

  def insert(v: V): Unit = {
    var changed = false
    index.updateWith(v) {
      case None =>
        changed = true
        Some(1)
      case Some(n) =>
        Some(n+1)
    }
    if (changed)
      notify(v, isInsertion = true)
  }

  def delete(v: V): Unit = {
    var changed = false
    index.updateWith(v) {
      case None => None
      case Some(n) =>
        if (n == 1) {
          changed = true
          None
        } else {
          Some(n-1)
        }
    }
    if (changed)
      notify(v, isInsertion = false)
  }

}
