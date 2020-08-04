package inca.runtime.index.unary

import inca.runtime.index.IndexKey

import scala.collection.mutable

class UnarySetIndex[V](val key: IndexKey[_]) extends UnaryIndex[V] {
  protected val index: mutable.Set[V] = mutable.Set()


  override def entries: Iterable[V] = index
  override def index(v: V): Int = if (index.contains(v)) 1 else 0

  def insert(v: V): Unit = {
    index += v
    notify(v, isInsertion = true)
  }

  def delete(v: V): Unit = {
    index -= v
    notify(v, isInsertion = false)
  }

}
