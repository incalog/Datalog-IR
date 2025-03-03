package inca.ir.extension.block.analysis.interpreter

import inca.ir
import inca.ir.*
import inca.ir.extension.block.Block
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, SupColumn}
import sturdy.data.MayJoin

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:

  override protected def canDetermineValue(t: Term): Boolean = t match
    case Block(_, t) => true
    case _ => super.canDetermineValue(t)

  override def evalTermOpen(term: Term)(using Fixed): SupColumn = term match
    case Block(ats, t) =>
      evalAtoms(ats)
      evalTerm(t)
    case _ => super.evalTermOpen(term)
