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
      val thnType = typechecker.typecheck(thn)(new TypeContext(context, cte))
      val elifTypes = elseIfs.map {elif =>
        val (condTyp, cte) = typechecker.typecheck(elif.cond)
        checkCond(condTyp)
        typechecker.typecheck(elif.body)(new TypeContext(context, cte))
      }
      // TODO lift this restriction later when the typechecker becomes more advanced
      if(els.isEmpty && (thnType +: elifTypes).exists(_ != TUnit)) {
        typechecker.errors.addOne(TypeError(s"attempt to yield value of type $thnType from If statement without else block"))
        return (None, context.tenv, true)
      }
      val elsType = els.fold[TypeAnno](TUnit)(typechecker.typecheck)
      if(!last) {
        (None, context.tenv, true)
      }
      else {
        val branchTypes = thnType +: elsType +: elifTypes
        val finalType = branchTypes.reduce[TypeAnno] {
          case (t1, t2) =>
            val commonType = typechecker.meet(t1, t2)
            commonType match {
              case None =>
                typechecker.errors.addOne(TypeError(s"The types $t1 and $t2 don't have a common type which is required at the end of a body"))
                return (None, context.tenv, true)
              case Some(t) =>
                t
            }
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
