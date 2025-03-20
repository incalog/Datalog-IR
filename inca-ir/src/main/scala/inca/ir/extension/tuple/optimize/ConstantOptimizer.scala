package inca.ir.extension.tuple.optimize

import inca.ir
import inca.ir.analysis.base.values.Value
import inca.ir.extension.tuple.analysis.interpreter.ConstantTupleV
import inca.ir.extension.tuple as irtuple
import inca.ir.*
import inca.ir.optimize.ConstantBaseIROptimizer

trait ConstantOptimizer extends ConstantBaseIROptimizer:

  override def valueToTermInternal(value: Value): Option[Term] = value match
    case ConstantTupleV(values) => Some(irtuple.TupleLit(values.flatMap(valueToTerm.apply)))
    case _ => super.valueToTermInternal(value)




