package inca.frontend.typechecker.extensions

import inca.frontend.core.Core
import inca.frontend.extensions.Cast
import inca.frontend.typechecker.CoreTypechecker.TypeEnvironment
import inca.frontend.typechecker.{TypeContext, TypeError, TypecheckerExtension}

object CastTypechecker extends TypecheckerExtension{

  override def typecheck(e: Core.Exp)(implicit context: TypeContext): (Core.TypeAnno, TypeEnvironment, Boolean) = e match {

    case Cast(src, targetTyp) =>
      val (srcType, _) = typechecker.typecheck(src)
      if(typechecker.subtype(srcType, targetTyp)) {
        (targetTyp, context.tenv, true)
      }
      else if(typechecker.subtype(targetTyp, srcType)) {
        (targetTyp, context.tenv, true)
      }
      else {
        typechecker.errors.addOne(TypeError(s"attempt to cast $srcType to unrelated type $targetTyp"))
        (Core.TUnit, context.tenv, false)
      }

    case _ => (Core.TUnit, context.tenv, false)
  }
}
