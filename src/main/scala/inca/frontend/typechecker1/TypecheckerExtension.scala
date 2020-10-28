package inca.frontend.typechecker1

import inca.frontend.core.Core._

/** Interface for Typechecker extensions
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
trait TypecheckerExtension {
  protected[typechecker1] var typechecker : CoreTypechecker = _ // Set by Typechecker instance.

  /**
   *
   * @param e the expression to be typechecked
   * @param context the type context
   * @return a triple of the computed type, the new type environment and a flag signalizing whether to use this type
   */
  def typecheck(e: Exp)(implicit
      context: TypeContext
  ): (TypeAnno, CoreTypechecker.TypeEnvironment, Boolean) = {
    (null, context.tenv, false)
  }

  def typecheck(s: Statement, last_in_body: Boolean)(implicit
      context: TypeContext
  ): (Option[TypeAnno], CoreTypechecker.TypeEnvironment, Boolean) = {
    (None, context.tenv, false)
  }
}
