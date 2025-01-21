package inca.ir.extension.data.optimize

import inca.ir
import inca.ir.analysis.base.values.Value
import inca.ir.extension.data.analysis.interpreter.ConstantDataV
import inca.ir.extension.data as irdata
import inca.ir.*
import inca.ir.optimize.ConstantBaseIROptimizer

trait ConstantOptimizer extends ConstantBaseIROptimizer:

  override def valueToTermInternal(value: Value): Option[Term] = value match
    case ConstantDataV(dataDef, caseDef, args) =>
      val argsV = args.flatMap(valueToTerm.apply)
      if (argsV.size != args.size)
        None
      else
        Some(irdata.Construct(caseDef.name, argsV))
    case _ => super.valueToTermInternal(value)



