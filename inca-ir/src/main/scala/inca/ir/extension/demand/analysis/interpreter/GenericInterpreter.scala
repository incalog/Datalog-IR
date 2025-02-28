package inca.ir.extension.demand.analysis.interpreter

import inca.ir
import inca.ir.*
import inca.ir.analysis.base.interpreter.BaseGenericInterpreter
import sturdy.data.MayJoin

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]

