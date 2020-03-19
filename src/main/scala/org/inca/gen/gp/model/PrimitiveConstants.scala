package org.inca.gen.gp.model

import org.inca.incer.IncrementalIndex
import org.inca.lang.core.Values.IValue

trait Primitive extends IValue {
  val value: Any
}

//@IncrementalIndex
case class BooleanConstant(value: Boolean) extends Primitive

