package inca.frontend.typechecker

import inca.frontend.core.Core._
import scala.collection.mutable.ArrayBuffer

/** Interface for Typechecker extensions
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
trait TypecheckerExtension {
  private[typechecker] var typechecker : Typechecker = _ // Set by Typechecker instance.

  def typecheck(e: Exp)(implicit
      context: TypeContext
  ): (TypeAnno, Typechecker.TypeEnvironment, Boolean)

  def typecheck(s: Statement, last_in_body: Boolean)(implicit
      context: TypeContext
  ): (Option[TypeAnno], Typechecker.TypeEnvironment, Boolean)
}
