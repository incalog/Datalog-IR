package org.inca.generators.gp.sdk.queryspecification

import scala.meta._

object util {
  def tempVarName(name: String): Term.Name = Term.Name(s"var__$name")
  def localParamName(name: String): Term.Name = Term.Name(s"var_$name")
  def stringToLit(s: String): Lit.String = Lit.String(s)
  def tempVarTermName(name: String): Pat.Var = Pat.Var(Term.Name(s"var__$name"))
  def paramVarName(name: String): Pat.Var = Pat.Var(Term.Name(s"p_$name"))
  def pVarVarName(name: String): Pat.Var = Pat.Var(Term.Name(s"var_$name"))
  def toTerm(s: String): Term.Name = Term.Name(s)
}
