package inca.frontend.typechecker.extensions

import inca.frontend.core.Core
import inca.frontend.core.Core.{Body, Exp, Name, TBool, TIterable, TLinked, TList}
import inca.frontend.extensions.{Exists, Forall}
import inca.frontend.typechecker.CoreTypechecker.TypeEnvironment
import inca.frontend.typechecker.{TypeContext, TypeError, TypecheckerExtension}

object ForAllExistsTypechecker extends TypecheckerExtension {

  override def typecheck(s: Core.Statement, last_in_body: Boolean)(implicit context: TypeContext): (Option[Core.TypeAnno], TypeEnvironment, Boolean) = s match {
    case Forall(name, exp, body) =>
      typecheck(name, exp, body, last_in_body)

    case Exists(name, exp, body) =>
      typecheck(name, exp, body, last_in_body)

    case _ => (None, context.tenv, false)
  }

  private def typecheck(name: Name, exp: Exp, body: Body, last: Boolean)(implicit context: TypeContext) = {
    val (expType, ete) = typechecker.typecheck(exp)
    expType match {
      case iterable: TIterable =>
        val contained = iterable.contained
        val bodyType = typechecker.typecheck(body)(new TypeContext(context, ete + (name -> contained)))
        if(bodyType != TBool) {
          typechecker.errors.addOne(TypeError(s"expected TBool, but got $bodyType"))
        }
      case _ =>
        typechecker.errors.addOne(TypeError(s"expected TIterable, bot got $expType"))
    }
    val resType = if(!last) None else Some(TBool)
    (resType, context.tenv, true)
  }
}
