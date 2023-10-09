package inca.backend.transform.monotype

import inca.runtime.aggregate.MonoAggregation

case class CountMono() extends MonoAggregation[Int, Int, Int] {
  override val name: String = ""

  override def init: Int = 0

  override def add(st : Int, a : Int) : Int = st + 1

  override def result(st : Int) : Int = st
}

class MonoTypes {

}
