package inca.frontend.typechecker.extensions

import inca.frontend.core.Core
import inca.frontend.core.Core._
import inca.frontend.extensions._
import inca.frontend.typechecker.{CoreTypechecker, TypeContext, TypeError, TypecheckerExtension}

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

        checkExp(e1t, e)
        checkExp(e2t, e)
        (typechecker.meet(e1t, e2t).get, typechecker.union(e1te, e2te), true)
      case Not(cond) =>
        val (condt, condte) = typechecker.typecheck(cond)

        if (!typechecker.subtype(condt, TBool))
          typechecker.addError(TypeError.expected(TBool, condt, s"Not ${cond.getClass.getName}"))

        (condt, condte, true)
      case Or(e1, e2) =>
        val (e1t, e1te) = typechecker.typecheck(e1)
        val (e2t, e2te) = typechecker.typecheck(e2)

        checkExp(e1t, e)
        checkExp(e2t, e)
        (typechecker.meet(e1t, e2t).get, typechecker.union(e1te, e2te), true)
      case _ => (null, context.tenv, false)
    }
  }

  private def checkExp(t: TypeAnno, exp: Exp)(implicit typeContext: TypeContext): Unit = {
    if(!typechecker.subtype(t, TBool)) {
      typechecker.addError(TypeError.expected(TBool, t, s"${exp.getClass.getName}"))
    }
  }
}
