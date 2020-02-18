package org.inca.gen

import org.inca.gen.gp.model.Primitive

import scala.meta.Lit

object Gensym {
  def generateLabel(prefix: String, value: Any): String = {
    value match {
      case _: String => s"$prefix$value"
      case _ => s"${prefix}_${value.hashCode().toString.replace('-', 'i')}"
    }
  }

  def primitiveLit(primitive: Primitive): Lit = {
    primitive.value match {
      case b: Boolean => Lit.Boolean(b)
      case i: Integer => Lit.Int(i)
      case s: String => Lit.String(s)
      case f: Float => Lit.Float(f)
    }
  }
}
