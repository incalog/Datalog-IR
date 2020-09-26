package inca.frontend.typechecker.extensions

import inca.frontend.core.Core
import inca.frontend.core.Core._
import inca.frontend.extensions._
import inca.frontend.typechecker.{CoreTypechecker, TypeContext, TypecheckerExtension}

/** BoolOps Typechecker Extension
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object BoolOpsTypechecker extends TypecheckerExtension {
  override def typecheck(e: Core.Exp)(implicit
      context: TypeContext
  ): (Core.TypeAnno, CoreTypechecker.TypeEnvironment, Boolean) = {
    e match {
      case And(e1, e2) =>
        val (e1t, e1te) = typechecker.typecheck(e1)
        val (e2t, e2te) = typechecker.typecheck(e2)

        if (!typechecker.subtype(e1t, TBool) || !typechecker.subtype(e2t, TBool))
          typechecker.addError(s"Cannot combine types $e1t and $e2t in an and operation.")
        (typechecker.meet(e1t, e2t).get, typechecker.union(e1te, e2te), true)
      case Not(cond) =>
        val (condt, condte) = typechecker.typecheck(cond)

        if (!typechecker.subtype(condt, TBool))
          typechecker.addError(s"Cannot negate type $condt.")

        (condt, condte, true)
      case Or(e1, e2) =>
        val (e1t, e1te) = typechecker.typecheck(e1)
        val (e2t, e2te) = typechecker.typecheck(e2)

        if (!typechecker.subtype(e1t, TBool) || !typechecker.subtype(e2t, TBool))
          typechecker.addError(s"Cannot combine types $e1t and $e2t in an or operation.")
        (typechecker.meet(e1t, e2t).get, typechecker.union(e1te, e2te), true)
      case _ => (null, context.tenv, false)
    }
  }
}
