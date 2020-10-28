package inca.frontend.typechecker1.extensions

import inca.frontend.core.Core
import inca.frontend.core.Core._
import inca.frontend.extensions._
import inca.frontend.typechecker1.{CoreTypechecker, TypeContext, TypecheckerExtension}

/** Switch Typechecker Extension
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object SwitchTypechecker extends TypecheckerExtension 
{
  override def typecheck(s: Core.Statement, last_in_body: Boolean)(implicit context: TypeContext): (Option[Core.TypeAnno], CoreTypechecker.TypeEnvironment, Boolean) = 
    s match {
      case Switch(bodies) =>
        val res = bodies.map(typechecker.typecheck(_))
        if (last_in_body) {
          val rest = res.foldLeft(Option[Core.TypeAnno](res.head))({case (a, b) => typechecker.meet(a.getOrElse(TAny), b)})
          (rest, context.tenv, true)
        }
        else
          (None, context.tenv, true)
      case _: CoreStatement => (None, context.tenv, false)
    }
}