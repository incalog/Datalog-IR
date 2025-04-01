package inca.ir.extension.demand.analysis.interpreter

import inca.ir
import inca.ir.analysis.base.effect.{BaseIRException, BaseIRFailure}
import inca.ir.analysis.base.values.{ConcreteRelation, Value}
import sturdy.data.MayJoin.NoJoin
import inca.ir

case object InvalidParam extends BaseIRFailure

trait ConcreteInterpreter extends GenericInterpreter[Value, Boolean, ConcreteRelation[Value], BaseIRException, NoJoin]:
  def provideValueForDemandedParam(param: ir.Param): Value =
    failure(InvalidParam, "Can not query a relation with unbound demanded parameter in top-down evaluation.")
