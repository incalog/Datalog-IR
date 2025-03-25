package inca.ir.extension.bool.optimize

import inca.ir
import inca.ir.*
import inca.ir.analysis.base.values.Value
import inca.ir.extension.bool as irbool
import inca.ir.extension.bool.AtomAsBool
import inca.ir.extension.bool.analysis.interpreter.ConstantBoolV
import inca.ir.optimize.ConstantBaseIROptimizer

trait ConstantOptimizer extends ConstantBaseIROptimizer:

  override def mayEliminate(term: Term): Boolean = term match
    case AtomAsBool(at) => visitAtom(at).isEmpty
    case _ => super.mayEliminate(term)

  override def valueToTermInternal(value: Value): Option[Term] = value match
    case ConstantBoolV(true) => Some(irbool.BoolTrue)
    case ConstantBoolV(false) => Some(irbool.BoolFalse)
    case _ => super.valueToTermInternal(value)




