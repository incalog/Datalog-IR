package inca.ir.extension.demand.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{AbstractRelation, BaseJoinV, BaseMeetV, Value}
import sturdy.values.Powerset
import sturdy.data.MayJoin
import sturdy.values.Topped
import sturdy.data.WithJoin
import inca.ir
import inca.ir.extension.data as irdata
import inca.ir.extension.data.analysis.interpreter.OODLClassV

trait OODLClassEqOps extends BaseEqOps

trait OODLClassJoinV extends BaseJoinV

trait OODLClassMeetV extends BaseMeetV

trait OODLClassAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]:
  def provideValueForDemandedParam(param: ir.Param): Value = param.ty match
    case irdata.TData(ref) if ref.name.name == "OID" || ref.name.name == "SID" => OODLClassV.Base
    case _ => Value.Top