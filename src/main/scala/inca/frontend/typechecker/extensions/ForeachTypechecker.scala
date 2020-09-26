package inca.frontend.typechecker.extensions

import inca.frontend.core.Core
import inca.frontend.core.Core.{TList, TUnit}
import inca.frontend.extensions.Foreach
import inca.frontend.typechecker.CoreTypechecker.TypeEnvironment
import inca.frontend.typechecker.{TypeContext, TypeError, TypeWarning, TypecheckerExtension}

object ForeachTypechecker extends TypecheckerExtension{

  override def typecheck(s: Core.Statement, last_in_body: Boolean)(implicit context: TypeContext): (Option[Core.TypeAnno], TypeEnvironment, Boolean) = s match {
    case Foreach(name, exp, body) =>
      val (expType, ete) = typechecker.typecheck(exp)
      expType match {
        case iterable: TList =>
          val bodyType = typechecker.typecheck(body)(new TypeContext(context, ete + (name -> iterable.contained)))
          if(bodyType != TUnit) {
            typechecker.addWarning(s"body has result type $bodyType which gets ignored")
          }
        case _ =>
          typechecker.addError(s"expected TList, but got $expType")
      }
      val resType = if(last_in_body) Some(TUnit) else None
      (resType, context.tenv, true)

    case _ => (None, context.tenv, false)
  }
}
