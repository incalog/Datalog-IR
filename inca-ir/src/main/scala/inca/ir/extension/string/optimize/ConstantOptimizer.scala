package inca.ir.extension.string.optimize

import inca.ir
import inca.ir.analysis.base.values.Value
import inca.ir.extension.string as irstr
import inca.ir.extension.string.analysis.interpreter.ConstantStringV
import inca.ir.*
import inca.ir.optimize.ConstantBaseIROptimizer

trait ConstantOptimizer extends ConstantBaseIROptimizer:

  override def mayEliminate(t: Term): Boolean = t match
    case irstr.StringLit(_) => isConstant(t)
    case irstr.StringConcat(lhs, rhs) =>
      isConstant(t) && isConstant(lhs) && isConstant(rhs)
      && mayEliminate(lhs) && mayEliminate(rhs)
    case irstr.ToString(tt) =>
      isConstant(t) && isConstant(tt) && mayEliminate(tt)
    case _ => super.mayEliminate(t)

  override def valueToTermInternal(value: Value): Option[Term] = value match
    case ConstantStringV(v) => Some(irstr.StringLit(v))
    case _ => super.valueToTermInternal(value)



