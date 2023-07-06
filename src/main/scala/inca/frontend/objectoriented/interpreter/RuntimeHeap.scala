package inca.frontend.objectoriented.interpreter

import java.lang
import scala.collection.mutable.ArrayBuffer

class RuntimeHeap {
  private var heapSize: Int = 0
  private val heap: ArrayBuffer[Object] = ArrayBuffer.empty
  @inline def apply(oid: Int): Object = heap(oid)
  @inline def update(oid: Int, o: Object) = heap(oid) = o
  def addObject(v: Int => Object): Int = {
    val oid = heapSize
    if (heap.size == heapSize)
      heap += v(oid)
    else
      heap(heapSize) = v(oid)
    heapSize += 1
    oid
  }
  def dropLast(oid: Int): Object = {
    if (oid == heapSize - 1) {
      val o = heap(oid)
      heapSize -= 1
      heap(heapSize) = null
      o
    } else {
      throw new IllegalArgumentException()
    }
  }
  def toImmutableHeap: ImmutableHeap = new ImmutableHeap(heap.toArray)
}

class ImmutableHeap(private val ar: Array[Object]) {
  private def oar: Array[lang.Object] = ar.asInstanceOf[Array[java.lang.Object]]

  override def hashCode(): Int = java.util.Arrays.hashCode(oar)

  override def equals(obj: Any): Boolean = obj match {
    case that: ImmutableHeap => java.util.Arrays.equals(oar, that.oar)
    case _ => false
  }

  override def toString: String = s"Heap(size=${ar.length}, ${java.util.Arrays.toString(oar)})"
}