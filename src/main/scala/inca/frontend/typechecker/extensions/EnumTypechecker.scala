package inca.frontend.typechecker.extensions

import inca.frontend.core.Core._
import inca.frontend.extensions._
import inca.frontend.typechecker.CoreTypechecker
import inca.frontend.typechecker.TypecheckerExtension
import inca.frontend.core.Core
import inca.frontend.typechecker.TypeContext
import inca.frontend.typechecker.TypeError

/** Enum Typechecker Extension
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object EnumTypechecker extends TypecheckerExtension {
    override def typecheck(e: Core.Exp)(implicit context: TypeContext): (Core.TypeAnno, CoreTypechecker.TypeEnvironment, Boolean) = {
        e match {
            case Enum(ty) => (ty, context.tenv, true)
            case _ => (null, context.tenv, false)
        }
    }
}
