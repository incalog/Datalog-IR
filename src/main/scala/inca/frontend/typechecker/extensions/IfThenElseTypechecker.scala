package inca.frontend.typechecker.extensions

import inca.frontend.core.Core
import inca.frontend.core.Core._
import inca.frontend.extensions.IfThenElse
import inca.frontend.typechecker.CoreTypechecker.TypeEnvironment
import inca.frontend.typechecker.{TypeContext, TypeError, TypeWarning, TypecheckerExtension}

object IfThenElseTypechecker extends TypecheckerExtension{

  override def typecheck(e: Core.Statement, last: Boolean)(implicit context: TypeContext):
  (Option[Core.TypeAnno], TypeEnvironment, Boolean) = e match {
    case IfThenElse(cond, thn, elseIfs, els) =>
      val (condTyp, cte) = typechecker.typecheck(cond)
      checkCond(condTyp)
      val thnType = typechecker.typecheck(thn)(new TypeContext(context, cte))
      val elifTypes = elseIfs.map {elif =>
        val (condTyp, cte) = typechecker.typecheck(elif.cond)
        checkCond(condTyp)
        typechecker.typecheck(elif.body)(new TypeContext(context, cte))
      }
      val branchTypes = thnType +: elifTypes
      val intermediate = branchTypes.reduce[TypeAnno] {
        case (t1, t2) =>
          val commonType = typechecker.meet(t1, t2)
          commonType match {
            case None =>
              typechecker.errors.addOne(TypeError(s"The types $t1 and $t2 don't have a common type"))
              return (None, context.tenv, true)
            case Some(t) =>
              t
          }
      }
      els match {
        case None =>
          if(last) {
            typechecker.errors.addOne(TypeError("incomplete IfThenElse at the end of the function"))
            (None, context.tenv, true)
          }
          else {
            // if the If statement would result in a type
            val finalType = if(intermediate == TUnit) None else Some(intermediate)
            (finalType, context.tenv, true)
          }
        case Some(elsBody) =>
          val elsType = typechecker.typecheck(elsBody)
          val finalType = typechecker.meet(intermediate, elsType) match {
            case None =>
              typechecker.errors.addOne(TypeError(s"The types $intermediate and $elsType don't have a common type"))
              return (Some(TUnit), context.tenv, true)
            case Some(t) => t
          }
          (Some(finalType), context.tenv, true)
      }

    case _ => (None, context.tenv, false)
  }

  private def checkCond(found: TypeAnno): Unit = {
    if(found != TBool) {
      typechecker.errors.addOne(TypeError(s"expected TBool, found $found"))
    }
  }
}
