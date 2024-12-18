package inca.ir.extension.string.optimize

import inca.ir
import inca.ir.analysis.base.values.Value
import inca.ir.extension.string as irstr
import inca.ir.extension.string.analysis.interpreter.ConstantStringV
import inca.ir.*
import inca.ir.optimize.ConstantBaseIROptimizer

trait ConstantOptimizer extends ConstantBaseIROptimizer:

  override def valueToTerm(value: Value): Option[Term] = value match
    case ConstantStringV(v) => Some(irstr.StringLit(v))
    case _ => super.valueToTerm(value)



