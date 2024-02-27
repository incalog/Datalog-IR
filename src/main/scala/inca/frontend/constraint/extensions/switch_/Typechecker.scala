package inca.frontend.constraint.extensions.switch_

import inca.frontend.constraint.core._
import inca.frontend.constraint.extensions.switch_.Trees._
import inca.frontend.constraint.typechecker.{CoreTypechecker, NoYield, StmType}

trait Typechecker extends CoreTypechecker {
  override protected def typecheckInternal(stm: Statement, mustYield: Boolean): StmType = stm match {
    case Switch(bodies) =>
      if (bodies.isEmpty) {
        error("empty switch statements are not allowed", stm)
        NoYield
      } else
        bodies.map(typecheck(_, mustYield)).reduce(stmMeet(_, _, dataModel))

    case _ => super.typecheckInternal(stm, mustYield)
  }
}
