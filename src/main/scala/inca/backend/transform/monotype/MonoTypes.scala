package inca.backend.transform.monotype

import inca.runtime.aggregate.MonoAggregation

import scala.collection.mutable

case class CountMono() extends MonoAggregation[Int, Int, Int] {
  override val name: String = ""

  override def init: Int = 0

  override def add(st : Int, a : Int) : Int = st + 1

  override def result(st : Int) : Int = st
}


case class AddMono() extends MonoAggregation[Int, Int, Int] {
  override val name: String = ""

  override def init: Int = 0

  override def add(st : Int, a : Int) : Int = st + a

  override def result(st : Int) : Int = st
}


case class ProdMono() extends MonoAggregation[Int, Int, Int] {
  override val name: String = ""

  override def init: Int = 1

  override def add(st : Int, a : Int) : Int = st * a

  override def result(st : Int) : Int = st
}

case class MapMono[ST, A, B, K](m : MonoAggregation[ST, A, B])
    extends MonoAggregation[mutable.Map[K, ST], (K, A), mutable.Map[K, B]] {
  override val name: String = "MapMono"
  override def init: mutable.Map[K, ST] = mutable.Map[K, ST]()

  override def add(st: mutable.Map[K, ST], a : (K, A)) : mutable.Map[K, ST] = {
    st(a._1) = m.add(if (st.contains(a._1)) st(a._1) else m.init, a._2)
    st
  }

  override def result(st: mutable.Map[K, ST]): mutable.Map[K, B] = {
    st map {case (k, v) => (k, m.result(v))}
  }
}

case class MaxMono(init : Int = 0) extends MonoAggregation[Int, Int, Int] {
  override val name: String = "MaxMono"

  override def add(st: Int, a: Int): Int = if (a > st) a else st

  override def result(st: Int): Int = st
}

class MonoTypes {

}
