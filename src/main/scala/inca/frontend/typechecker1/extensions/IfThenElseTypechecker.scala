package inca.frontend.typechecker1.extensions

import inca.frontend.core.Core
import inca.frontend.core.Core._
import inca.frontend.extensions.IfThenElse
import inca.frontend.typechecker1.CoreTypechecker.TypeEnvironment
import inca.frontend.typechecker1.{TypeContext, TypeError, TypecheckerExtension}

/**
 * IfThenElse typechecker extension
 *
 * @author Ronja Schnur (rschnur@students.uni-mainz.de)
 *         Julian Cichorius (jcichori@students.uni-mainz.de)
 */
object IfThenElseTypechecker extends TypecheckerExtension{

  override def typecheck(e: Core.Statement, last: Boolean)(implicit context: TypeContext):
  (Option[Core.TypeAnno], TypeEnvironment, Boolean) = e match {
    case IfThenElse(cond, thn, elseIfs, els) =>
      //If
      val (condTyp, cte) = typechecker.typecheck(cond)
      checkCond(condTyp)
      val thnType = typechecker.typecheck(thn)(new TypeContext(context, cte))
      //ElseIf
      val elifTypes = elseIfs.map {elif =>
        val (condTyp, cte) = typechecker.typecheck(elif.cond)
        checkCond(condTyp)
        typechecker.typecheck(elif.body)(new TypeContext(context, cte))
      }
      val branchTypes = thnType +: elifTypes
      val intermediate = typechecker.meet(branchTypes).fold[TypeAnno](TUnit)(t => t)
      //Else
      els match {
        case None =>
          if(last) {
            typechecker.addError(TypeError.incompleteStatement(e, "IfThenElse"))
            (None, context.tenv, true)
          }
          else {
            // if the If statement would result in a type we have to account for that in the type computation of the whole body
            val finalType = if(intermediate == TUnit) None else Some(intermediate)
            (finalType, context.tenv, true)
          }
        case Some(elsBody) =>
          val elsType = typechecker.typecheck(elsBody)
          val finalType = typechecker.meet(intermediate, elsType) match {
            case None =>
              typechecker.addError(TypeError.incompatibleReturnTypes("IfThenElse"))
              return (Some(TUnit), context.tenv, true)
            case Some(t) => t
          }
          (Some(finalType), context.tenv, true)
      }

    case _ => (None, context.tenv, false)
  }

  private def checkCond(found: TypeAnno)(implicit ctx: TypeContext): Unit = {
    if(!typechecker.subtype(found, TBool)) {
      typechecker.addError(TypeError.expected(TBool, found, "IfThenElse"))
    }
  }
}
