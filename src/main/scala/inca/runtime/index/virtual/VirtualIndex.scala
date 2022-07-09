package inca.runtime.index.virtual

import inca.runtime.db.Database
import inca.runtime.index.binary.BinaryIndex
import inca.runtime.index.unary.UnaryIndex
import inca.runtime.index.BagIndex
import inca.runtime.index.Index
import inca.runtime.index.IndexKey

/** A virtual index stores no data of its own but uses other indices to answer queries */
trait VirtualIndex extends Index {

  protected var database: Database = _
  private[virtual] def setDatabase(database: Database): Unit = {
    this.database = database
    afterInitialization()
  }

  def afterInitialization(): Unit
}

trait VirtualUnaryIndex[V] extends UnaryIndex[V] with VirtualIndex {
  override def insert(v: V): Unit = throw new UnsupportedOperationException
  override def delete(v: V): Unit = throw new UnsupportedOperationException
}

trait VirtualBinaryIndex[K, V] extends BinaryIndex[K, V] with VirtualIndex {
  override def insert(k: K, v: V): Unit = throw new UnsupportedOperationException
  override def delete(k: K, v: V): Unit = throw new UnsupportedOperationException
  override def update(k: K, vold: V, vnew: V): Unit = throw new UnsupportedOperationException
}

abstract class VirtualBagIndex(key: IndexKey[_]) extends BagIndex(key) with VirtualIndex {
  override def insert(v: Tuple): Unit = throw new UnsupportedOperationException
  override def delete(v: Tuple): Unit = throw new UnsupportedOperationException
}
