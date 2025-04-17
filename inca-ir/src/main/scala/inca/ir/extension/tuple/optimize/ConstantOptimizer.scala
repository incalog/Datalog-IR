package inca.ir.extension.tuple.optimize

import inca.ir
import inca.ir.analysis.base.values.Value
import inca.ir.extension.tuple.analysis.interpreter.ConstantTupleV
import inca.ir.extension.tuple as irtuple
import inca.ir.*
import inca.ir.optimize.ConstantBaseIROptimizer

trait ConstantOptimizer extends ConstantBaseIROptimizer:

  override def mayEliminate(t: Term): Boolean = t match
    case irtuple.TupleLit(ts) => isConstant(t) && ts.forall(isConstant) && ts.forall(mayEliminate)
    case irtuple.Project(tt, _) => isConstant(t) && isConstant(tt) && mayEliminate(tt)
    case _ => super.mayEliminate(t)

  override def valueToTermInternal(value: Value): Option[Term] = value match
    case ConstantTupleV(values) =>
      val newValues = values.flatMap(valueToTerm.apply)
      if (newValues.size == values.size)
        Some(irtuple.TupleLit(newValues))
      else
        None
    case _ => super.valueToTermInternal(value)




