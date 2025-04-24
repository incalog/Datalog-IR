package inca.ir.extension.demand.optimize

import inca.ir
import inca.ir.extension.demand as irdemand
import inca.ir.*
import inca.ir.optimize.ConstantBaseIROptimizer

trait ConstantOptimizer extends ConstantBaseIROptimizer:

  // we can not cast to TDemand
  override def cast(t: Term, ty: Type): Cast = ty match
    case irdemand.TDemand(tty) => Cast(t, tty)
    case _ => super.cast(t, ty)



