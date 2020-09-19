package inca.frontend.typechecker.extensions

import inca.frontend.core.Core._
import inca.frontend.extensions._
import inca.frontend.typechecker.CoreTypechecker
import inca.frontend.typechecker.TypecheckerExtension
import inca.frontend.core.Core
import inca.frontend.typechecker.TypeContext
import inca.frontend.typechecker.TypeError

/** IfThenElse Typechecker Extension
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object IfThenElseTypechecker extends TypecheckerExtension {
  def typecheck(eif: ElseIf)(implicit context: TypeContext): TypeAnno = {
    val ElseIf(cond, body) = eif
    val (ct, cte) = typechecker.typecheck(cond)
    if (!typechecker.subtype(ct, TBool))
      typechecker.errors.addOne(
        TypeError(
          s"Else If condition does not evaluate to Boolean (${typechecker.where})."
        )
      )
    typechecker.typecheck(body)(new TypeContext(context, cte))
  }

  override def typecheck(s: Core.Statement, last_in_body: Boolean)(implicit
      context: TypeContext
  ): (Option[Core.TypeAnno], CoreTypechecker.TypeEnvironment, Boolean) =
    s match {
      case IfThenElse(cond, thn, elseIfs, els) => {
        val (ct, cte) = typechecker.typecheck(cond)

        var return_types = Seq(
          typechecker.typecheck(thn)(new TypeContext(context, cte))
        ) ++ elseIfs.map(typecheck(_)) ++
          (els match {
            case Some(value) => Seq(typechecker.typecheck(value))
            case None        => Seq.empty[TypeAnno]
          })

        if (last_in_body) {
          // check if all return values are the same
          if (return_types.exists(x => !typechecker.subtype(return_types.head, x)))
            typechecker.errors.addOne(
              TypeError(
                s"Body has multiple return values (${typechecker.where})."
              )
            )

          val rt =
            if (return_types.nonEmpty) return_types.head
            else TUnit

          (Some(rt), context.tenv, true)
        } else
          (None, context.tenv, true)

      }
      case _ => (None, context.tenv, false)
    }
}
