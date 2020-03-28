package org.inca.gen.gp.model

import org.inca.incer.IncrementalIndex
import org.inca.lang.core.Values.Value
import org.inca.meta.MetaElements.MetaElement

trait Primitive extends Value {
  val value: Any
}

@IncrementalIndex
case class BooleanConstant(value: Boolean) extends Primitive

@IncrementalIndex
case class IntegerConstant(value: Int) extends Primitive

@IncrementalIndex
case class LongConstant(value: Long) extends Primitive

