package inca.ir.extension.block.optimize

import inca.ir
import inca.ir.extension.block as irblock
import inca.ir.*
import inca.ir.analysis.base.values.Value
import inca.ir.optimize.ConstantBaseIROptimizer

trait ConstantOptimizer extends ConstantBaseIROptimizer:

  // blocks may fail, we can not eliminate them if they contain an atom
  override def mayEliminate(term: Term): Boolean = term match
    case irblock.Block(ats, t) => ats.flatMap(visitAtom).isEmpty && mayEliminate(t)
    case _ => super.mayEliminate(term)



