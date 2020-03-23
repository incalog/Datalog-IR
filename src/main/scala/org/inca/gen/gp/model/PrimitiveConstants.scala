package org.inca.gen.gp.model

import org.inca.incer.IncrementalIndex
import org.inca.lang.core.Values.IValue
import org.inca.meta.MetaElements.MetaElement

trait Primitive extends IValue with MetaElement {
  val value: Any
}

@IncrementalIndex
case class BooleanConstant(value: Boolean) extends Primitive

