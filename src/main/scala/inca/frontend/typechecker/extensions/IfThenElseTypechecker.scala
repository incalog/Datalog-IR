package inca.frontend.typechecker.extensions

import inca.frontend.core.Core
import inca.frontend.core.Core._
import inca.frontend.extensions.IfThenElse
import inca.frontend.typechecker.CoreTypechecker.TypeEnvironment
import inca.frontend.typechecker.{TypeContext, TypeError, TypecheckerExtension}

object IfThenElseTypechecker extends TypecheckerExtension{

  override def typecheck(e: Core.Statement, last: Boolean)(implicit context: TypeContext):
  (Option[Core.TypeAnno], TypeEnvironment, Boolean) = e match {
    case IfThenElse(cond, thn, elseIfs, els) =>
      val (condTyp, cte) = typechecker.typecheck(cond)
      checkCond(condTyp)
      val thnType = typechecker.typecheck(thn)
      val (elifTypes, envs) = elseIfs.map(elif => {
        val (found, te) = typechecker.typecheck(elif.cond)
        checkCond(found)
        val bodyTyp = typechecker.typecheck(elif.body)
        (bodyTyp, te)
      }).unzip
      val finalEnv = envs.fold(cte) {
        case (env1, env2) => typechecker.union(env1, env2)
      }
      val intermediateType = elifTypes.fold(thnType) {
        case (typ1, typ2) =>
          if(typechecker.subtype(typ1, typ2)) {
            typ2
          }else if(typechecker.subtype(typ2, typ1)){
            typ1
          }else {
            // the types are unrelated so the whole expression can't be typed
            els.foreach(typechecker.typecheck)
            if(last) {
              return (Some(TUnit), finalEnv, true)
            }
            else {
              return (None, finalEnv, true)
            }
          }
      }
      val finalType = els.fold[Option[TypeAnno]](Some(TUnit)) {body =>
        val eType = typechecker.typecheck(body)
        if(typechecker.subtype(intermediateType, eType)) {
          Some(eType)
        } else if(typechecker.subtype(eType, intermediateType)) {
          Some(intermediateType)
        }else {
          if(last) {
            Some(TUnit)
          }
          else {
            None
          }
        }
      }
      (finalType, finalEnv, true)

    case _ => (None, context.tenv, false)
  }

  private def checkCond(found: TypeAnno): Unit = {
    if(found != TBool) {
      typechecker.errors.addOne(TypeError(s"expected TBool, found $found"))
    }
  }
}
