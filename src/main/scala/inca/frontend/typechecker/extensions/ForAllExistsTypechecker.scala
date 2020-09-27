package inca.frontend.typechecker.extensions

import inca.frontend.core.Core
import inca.frontend.core.Core._
import inca.frontend.extensions.{Exists, Forall}
import inca.frontend.typechecker.CoreTypechecker.TypeEnvironment
import inca.frontend.typechecker.{TypeContext, TypeError, TypecheckerExtension}

/**
 * ForallExists typechecker extension
 *
 * @author Ronja Schnur (rschnur@students.uni-mainz.de)
 *         Julian Cichorius (jcichori@students.uni-mainz.de)
 */
object ForAllExistsTypechecker extends TypecheckerExtension {

  override def typecheck(s: Core.Statement, last_in_body: Boolean)(implicit context: TypeContext): (Option[Core.TypeAnno], TypeEnvironment, Boolean) = s match {
    case Forall(name, exp, body) =>
      typecheck(name, exp, body, "Forall")

    case Exists(name, exp, body) =>
      typecheck(name, exp, body, "Exists")

    case _ => (None, context.tenv, false)
  }

  private def typecheck(name: Name, exp: Exp, body: Body, sName: String)(implicit context: TypeContext) = {
    val (expType, ete) = typechecker.typecheck(exp)
    expType match {
      case iterable: TList =>
        val bodyType = typechecker.typecheck(body)(new TypeContext(context, ete + (name -> iterable.contained)))
        if(bodyType != TUnit) {
          typechecker.addWarning(s"body has result type $bodyType which gets ignored")
        }
      case _ =>
        typechecker.addError(TypeError.expected(TList(TAnyLinked), expType, s"$sName $name <- ${exp.getClass.getName}"))
    }
    (Some(TUnit), context.tenv, true)
  }
}
