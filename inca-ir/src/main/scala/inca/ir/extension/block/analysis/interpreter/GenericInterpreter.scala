package inca.ir.extension.block.analysis.interpreter

import inca.ir
import inca.ir.*
import inca.ir.extension.block.Block
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, SupColumn}
import sturdy.data.MayJoin

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:

  override protected def canDetermineValue(t: Term): Boolean = t match
    case Block(_, _) => true
    case _ => super.canDetermineValue(t)

  override def evalTermOpen(term: Term)(using Fixed): SupColumn = term match
    case Block(ats, t) =>
      evalAtoms(ats)
      val termRes = evalTerm(t)
      // while technically not necessary, we copy the result to a new column to nicely separate the block result
      // from its encapsulated term result. This is also necessary for the annotator to work correctly.
      val resultCol = gensym.fresh("result")
      updateSupplementaryChecked { sup =>
        relationOps.copyColumn(sup, termRes, resultCol)
      }
      resultCol
    case _ => super.evalTermOpen(term)
