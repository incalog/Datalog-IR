package inca.frontend.extensions.ifThenElse

import inca.frontend.core.tree._
import inca.frontend.extensions.ifThenElse.Trees._
import inca.frontend.typechecker.{CoreTypechecker, NoYield, StmType}

trait Typechecker extends CoreTypechecker {
  override protected def typecheckInternal(stm: Statement, mustYield: Boolean): StmType = stm match {
    case IfThenElse(cond, thn, elseIfs, els) =>
      val conds = cond +: elseIfs.map(_.cond)
      conds.foreach { c =>
        val ty = typecheck(c)
        if (!subtype(ty, TScalaBoolean, lang))
          error(s"Expected Boolean condition, but got $ty", c)
      }

      val bodies = thn +: elseIfs.map(_.body)
      val bodyTypes = bodies.map { b =>
        typecheck(b, mustYield)
      }

      val elsTy = els.map(typecheck(_, mustYield)).getOrElse(NoYield)
      bodyTypes.foldLeft(elsTy)(stmMeet(_, _, lang))

    case _ => super.typecheckInternal(stm, mustYield)
  }
}
