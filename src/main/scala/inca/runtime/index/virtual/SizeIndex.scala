package inca.runtime.index.virtual

import inca.runtime.index.binary.BinaryIndex
import inca.runtime.index.dynamic.ParentIndex
import inca.runtime.index.{IndexKey, VirtualKey}
import inca.util.TupleOps
import org.eclipse.viatra.query.runtime.matchers.tuple.{ITuple, Tuple, TupleMask, Tuples}
import truechange._

object SizeIndex {
  case object Key extends VirtualKey {
    override val getStringID: String = "#size"
    override val getArity: Int = 2
    override def isEnumerable: Boolean = true
  }

  def apply(): (VirtualKey, SizeIndex) =
    Key -> new SizeIndex()
}


class SizeIndex extends BinaryIndex[URI, Int]
  with VirtualIndex {

  lazy val parentIndex: ParentIndex = database.dynamicIndices.getOrElse(ParentIndex.Key, throw new IllegalStateException("Size index requires parent index to be present")).asInstanceOf[ParentIndex]
  def sizeOf(node: URI): Int = parentIndex.getChildren(node).size

  override def insert(k: URI, v: Int): Unit = throw new UnsupportedOperationException(s"Cannot insert tuple into virtual index $this")
  override def delete(k: URI, v: Int): Unit = throw new UnsupportedOperationException(s"Cannot delete tuple from virtual index $this")


  /** The key of this index */
  override val key: IndexKey[_] = SizeIndex.Key

  /** checks whether the provided tuple is contained in index */
  override def containsTuple(tuple: ITuple): Boolean = {
    if (tuple == null)
      return false

    val node = tuple.get(0).asInstanceOf[URI]
    val size = tuple.get(1).asInstanceOf[Int]
    sizeOf(node) == size
  }

  /** counts tuples of the associated virtual key contained in index based on provided mask and seed */
  final override def countTuples(mask: TupleMask, seed: ITuple): Int = {
    val maskLength = mask.indices.length
    if (maskLength == 0) {
      parentIndex.getEntries.size
    } else if (maskLength == 1) {
      val isOrdered = mask.indices(0) == 0
      if (isOrdered) {
        1
      } else {
        val size = seed.get(1).asInstanceOf[Int]
        parentIndex.getEntries.count(kv => kv._2.size == size)
      }
    } else if (maskLength == 2) {
      val isOrdered = mask.indices(0) == 0
      if (isOrdered && containsTuple(seed)) {
        1
      } else if (!isOrdered && containsTuple(TupleOps.binaryFlip(seed))) {
        1
      } else {
        0
      }
    } else {
      throw new IllegalArgumentException("Invalid tuple mask " + mask + " for bijective virtual index " + this)
    }
  }

  /** returns all tuples maintained in index associated with virtual key based on provided mask and seed  */
  final override def enumerateTuples(mask: TupleMask, seed: ITuple): Iterable[Tuple] = {
    val maskLength = mask.indices.length
    if (maskLength == 0) {
      parentIndex.getEntries.map(kv => Tuples.staticArityFlatTupleOf(kv._1, kv._2.size))
    } else if (maskLength == 1) {
      val isOrdered = mask.indices(0) == 0
      if (isOrdered) {
        val node = seed.get(0).asInstanceOf[URI]
        val size = sizeOf(node)
        Iterable(Tuples.staticArityFlatTupleOf(node, size))
      } else {
        val size = seed.get(1).asInstanceOf[Int]
        parentIndex.getEntries.filter(kv => kv._2.size == size).map(kv => Tuples.staticArityFlatTupleOf(kv._1, kv._2))
      }
    } else if (maskLength == 2) {
      val isOrdered = mask.indices(0) == 0
      if (isOrdered && containsTuple(seed)) {
        Seq(TupleOps.binaryTuple(seed))
      } else if (!isOrdered && containsTuple(TupleOps.binaryFlip(seed))) {
        Seq(TupleOps.binaryTuple(seed))
      } else {
        Seq()
      }
    } else {
      throw new IllegalArgumentException("Invalid tuple mask " + mask + " for bijective virtual index " + this)
    }
  }

  /** enumerate all values within index associated with virtual key based on provided mask and seed */
  final override def enumerateValues(mask: TupleMask, seed: ITuple): Iterable[Tuple] = {
    val maskLength = mask.indices.length
    if (maskLength == 1) {
      val isOrdered = mask.indices(0) == 0
      if (isOrdered) {
        val node = seed.get(0).asInstanceOf[URI]
        val size = sizeOf(node)
        Iterable(Tuples.staticArityFlatTupleOf(node, size))
      } else {
        val size = seed.get(1).asInstanceOf[Int]
        parentIndex.getEntries.filter(kv => kv._2.size == size).map(kv => Tuples.staticArityFlatTupleOf(kv._1, kv._2))
      }
    } else {
      throw new IllegalArgumentException("Invalid tuple mask " + mask + " for enumerateValues in bijective virtual index " + this)
    }
  }
}
