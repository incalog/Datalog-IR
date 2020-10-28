package inca.frontend.typechecker1.extensions

import inca.frontend.core.Core
import inca.frontend.core.Core.{TAnyLinked, TList, TUnit}
import inca.frontend.extensions.Foreach
import inca.frontend.typechecker1.CoreTypechecker.TypeEnvironment
import inca.frontend.typechecker1.{TypeContext, TypeError, TypecheckerExtension}

/**
 * Foreach typechecker extension
 *
 * @author Ronja Schnur (rschnur@students.uni-mainz.de)
 *         Julian Cichorius (jcichori@students.uni-mainz.de)
 */
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
          typechecker.addError(TypeError.expected(TList(TAnyLinked), expType, s"Foreach $name <- ${exp.getClass.getName}"))
      }
      val resType = if(last_in_body) Some(TUnit) else None
      (resType, context.tenv, true)

    case _ => (None, context.tenv, false)
  }
}
