package org.inca.gen


import org.inca.lang.Values.LiteralValue

import scala.collection.mutable


object Gensym {
  val variables: mutable.Map[LiteralValue, String] = mutable.Map[LiteralValue, String]()

  def register(literal: LiteralValue): Unit = {
    if (!variables.contains(literal)) {
      variables.addOne((literal, java.util.UUID.randomUUID.hashCode().toString.replace('-', 'i')))
    }
  }
}
