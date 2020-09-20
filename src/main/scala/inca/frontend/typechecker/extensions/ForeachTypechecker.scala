package inca.frontend.typechecker.extensions

import inca.frontend.core.Core
import inca.frontend.core.Core.{TIterable, TUnit}
import inca.frontend.extensions.Foreach
import inca.frontend.typechecker.CoreTypechecker.TypeEnvironment
import inca.frontend.typechecker.{TypeContext, TypeError, TypecheckerExtension}

object ForeachTypechecker extends TypecheckerExtension{

  override def typecheck(s: Core.Statement, last_in_body: Boolean)(implicit context: TypeContext): (Option[Core.TypeAnno], TypeEnvironment, Boolean) = s match {
    case Foreach(name, exp, body) =>
      val (expType, ete) = typechecker.typecheck(exp)
      expType match {
        case iterable: TIterable =>
          val contained = iterable.contained
          typechecker.typecheck(body)(new TypeContext(context, ete + (name -> contained)))
        case _ =>
          typechecker.errors.addOne(TypeError(s"expected TIterable, but got $expType"))
      }
      val resType = if(last_in_body) Some(TUnit) else None
      (resType, context.tenv, true)

    case _ => (None, context.tenv, false)
  }
}
