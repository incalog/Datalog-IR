package org.inca.gen.gp.model

import org.inca.incer.IncrementalIndex
import org.inca.lang.Core.Value

trait Primitive extends Value {
  val value: Any
}

case class BooleanConstant(value: Boolean) extends Primitive

case class IntegerConstant(value: Int) extends Primitive

case class LongConstant(value: Long) extends Primitive

