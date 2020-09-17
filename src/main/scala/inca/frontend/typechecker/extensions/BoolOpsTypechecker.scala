package inca.frontend.typechecker.extensions

import inca.frontend.core.Core._
import inca.frontend.extensions._ 
import inca.frontend.typechecker.CoreTypechecker
import inca.frontend.typechecker.TypecheckerExtension
import inca.frontend.core.Core
import inca.frontend.typechecker.TypeContext
import inca.frontend.typechecker.TypeError

object BoolOpsTypechecker extends TypecheckerExtension
{
    override def typecheck(e: Core.Exp)(implicit context: TypeContext): (Core.TypeAnno, CoreTypechecker.TypeEnvironment, Boolean) = {
        e match {
            case And(e1, e2) => {
                val (e1t, e1te) = typechecker.typecheck(e1)
                val (e2t, e2te) = typechecker.typecheck(e2)

                if (e1t != TBool || e2t != TBool) // @todo subtyping
                    typechecker.errors.addOne(TypeError(s"Cannot combine types $e1t and $e2t in an and operation (${typechecker.where})."))
                (TBool, e1te ++ e2te ++ context.tenv, true)
            }
            case Not(cond) => {
                val (condt, condte) = typechecker.typecheck(cond)

                if (condt != TBool) // @todo subtyping 
                    typechecker.errors.addOne(TypeError(s"Cannot negate type $condt (${typechecker.where})."))
                
                (TBool, condte ++ context.tenv, true)
            }
            case Or(e1, e2) => {
                val (e1t, e1te) = typechecker.typecheck(e1)
                val (e2t, e2te) = typechecker.typecheck(e2)

                if (e1t != TBool || e2t != TBool) // @todo subtyping
                    typechecker.errors.addOne(TypeError(s"Cannot combine types $e1t and $e2t in an or operation (${typechecker.where})."))
                (TBool, e1te ++ e2te ++ context.tenv, true)
            }
            case _ => (null, context.tenv, false)
        }
    }
}