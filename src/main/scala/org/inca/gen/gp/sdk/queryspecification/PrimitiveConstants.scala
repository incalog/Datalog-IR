package org.inca.gen.gp.sdk.queryspecification

import org.inca.lang.core.Values.IValue

object PrimitiveConstants {
  trait Primitive extends IValue {
    val value: Any
  }

  case class BooleanConstant(value: Boolean) extends Primitive
}
