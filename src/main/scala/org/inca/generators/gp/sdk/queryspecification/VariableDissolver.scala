package org.inca.generators.gp.sdk.queryspecification

import org.inca.generators.gp.sdk.queryspecification.Primitives._
import org.inca.generators.gp.util.Util._

import scala.meta._


object VariableDissolver {

  def getLabel(primitive: Primitive): String = {
    primitive match {
      case _: FloatPrimitive =>
        primitive.value.toString.replace('.', '_').appended('f')
      case _ =>
        primitive.value.toString
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

  def asParam(s: String): String = "p_" + s
  def asBodyVar(s: String): String = "var_" + s
  def asVar(s: String): String = "var__" + s

  implicit def metaMagic(s: String): MagicMeta = new MagicMeta(s)
}

// wird umbenannt, kein Angst ;)
class MagicMeta(s: String) {
  def toTerm: Term.Name = Term.Name(s)
  def toLit: Lit.String = Lit.String(s)
  def toVar: Pat.Var = Pat.Var(Term.Name(s))
  def toType: Type.Name = Type.Name(s)
  def toClassPath: Type.Select = classPathToTypeSelect(s)
}
