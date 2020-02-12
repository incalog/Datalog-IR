package org.inca.gen.gp.sdk.queryspecification

import org.inca.gen.gp.util.Util._

import scala.meta._

// todo rmv file
object VariableDissolver {

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
