package org.inca.generators.gp.sdk.queryspecification

import org.inca.generators.gp.sdk.queryspecification.Primitives._

import scala.meta.{Lit, Pat, Term}


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


  def tempVarName(name: String): Term.Name = Term.Name(s"var__$name")
  def localParamName(name: String): Term.Name = Term.Name(s"var_$name")
  def stringToLit(s: String): Lit.String = Lit.String(s)
  def tempVarTermName(name: String): Pat.Var = Pat.Var(Term.Name(s"var__$name"))
  def paramVarName(name: String): Pat.Var = Pat.Var(Term.Name(s"p_$name"))
  def pVarVarName(name: String): Pat.Var = Pat.Var(Term.Name(s"var_$name"))
  def toTerm(s: String): Term.Name = Term.Name(s)
}
