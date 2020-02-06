package org.inca.generators.gp.sdk.queryspecification

import org.inca.lang.core.Values.IValue

object Primitives {
  trait Primitive extends IValue {
    val value: Any
  }

  case class BoolPrimitive(value: Boolean) extends Primitive
  case class IntPrimitive(value: Int) extends Primitive
  case class FloatPrimitive(value: Float) extends Primitive
  case class StringPrimitive(value: String) extends Primitive
}
