package inca.frontend.constraint.extensions.forallExists

import inca.frontend.constraint.core._
import inca.frontend.constraint.extensions.forallExists.Trees._
import inca.frontend.constraint.typechecker.{CoreTypechecker, NoYield, StmType}

trait Typechecker extends CoreTypechecker {
  override protected def typecheckInternal(stm: Statement, mustYield: Boolean): StmType = stm match {
    case forall@Forall(name, exp, body) =>
      val ety = typecheck(exp)
      val elemType = ety match {
        case it: TIterable => it.contained
        case _ =>
          error(s"Found $ety, but expected iterable type", exp)
          TAny
      }
      scopedTypeContext {
        bindVar(name, forall, elemType)
        typecheck(body, mustYield = false)
      }
      NoYield

    case ex@Exists(name, exp, body) =>
      val ety = typecheck(exp)
      val elemType = ety match {
        case it: TIterable => it.contained
        case _ =>
          error(s"Found $ety, but expected iterable type", exp)
          TAny
      }
      scopedTypeContext {
        bindVar(name, ex, elemType)
        typecheck(body, mustYield = false)
      }
      NoYield

    case _ => super.typecheckInternal(stm, mustYield)
  }
}
