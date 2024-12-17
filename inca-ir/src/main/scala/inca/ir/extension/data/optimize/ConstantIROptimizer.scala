package inca.ir.extension.data.optimize

import inca.ir
import inca.ir.analysis.base.values.Value
import inca.ir.extension.data.analysis.interpreter.ConstantDataV
import inca.ir.extension.data as irdata
import inca.ir.*
import inca.ir.optimize.ConstantBaseIROptimizer

trait ConstantIROptimizer extends ConstantBaseIROptimizer:

  override def valueToTerm(value: Value): Option[Term] = value match
    case ConstantDataV(dataDef, caseDef, args) =>
      val argsV = args.flatMap(valueToTerm)
      if (argsV.size != args.size)
        None
      else
        Some(irdata.Construct(caseDef.name, argsV))
    case _ => super.valueToTerm(value)



