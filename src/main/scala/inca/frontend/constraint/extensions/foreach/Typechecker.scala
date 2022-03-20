package inca.frontend.constraint.extensions.foreach

import inca.frontend.constraint.core._
import inca.frontend.constraint.extensions.foreach.Trees._
import inca.frontend.constraint.typechecker.CoreTypechecker
import inca.frontend.constraint.typechecker.StmType

trait Typechecker extends CoreTypechecker {
  override protected def typecheckInternal(stm: Statement, mustYield: Boolean): StmType =
    stm match {
      case foreach @ Foreach(name, exp, body) =>
        val ety = typecheck(exp)
        val elemType = ety match {
          case it: TIterable => it.contained
          case _ =>
            error(s"Found $ety, but expected iterable type", exp)
            TAny
        }

        scopedTypeContext {
          bindVar(name, foreach, elemType)
          typecheck(body, mustYield)
        }

      case _ => super.typecheckInternal(stm, mustYield)
    }
}
