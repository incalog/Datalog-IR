package inca.ir.extension.block.optimize

import inca.ir
import inca.ir.extension.block as irblock
import inca.ir.*
import inca.ir.optimize.ConstantBaseIROptimizer

trait ConstantOptimizer extends ConstantBaseIROptimizer:

  // blocks may fail, we can not eliminate them
  override def mayEliminate(t: Term): Boolean = t match
    case irblock.Block(at, t) => false
    case _ => super.mayEliminate(t)



